{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards   #-}


module Handler.Crypto where

import Import
import Types.BTC
import Text.Read                    as RT
import Prelude                      as P        (read, readFile, map)
import Data.Aeson                   as AS       (encode, decode)
import Data.Text                    as T        (pack, unpack)
import Data.Bifunctor               as BF       (Bifunctor(first) ) 
import Data.ByteString.Lazy.UTF8    as BSU
import Data.Map.Strict              as STS      (lookup, toList, fromList)
import Network.HTTP.Req             as REQ
    ( (/:),
      defaultHttpConfig,
      https,
      jsonResponse,
      req,
      responseBody,
      runReq,
      GET(GET),
      NoReqBody(NoReqBody) )

import Types.Currency

-------------------------------------------------------
-- | Route handlers
-------------------------------------------------------
getCryptoR :: Handler Value
getCryptoR = do
  (BTC time disclaimer bpi) <- Import.liftIO middlewareRequest

  let response = object [
                    "time" .= time
                  , "disclaimer" .= disclaimer
                  , "bpi" .= bpi
                ]

  returnJson response

postCryptoR :: Handler Value
postCryptoR = do
  (NewCurrencyValue textCurrencyCode newvalue) <- (requireCheckJsonBody :: Handler NewCurrencyValue)

  print $ stringToCurrencyValue $ T.unpack textCurrencyCode
  
  let mCurrency = stringToCurrencyValue (T.unpack textCurrencyCode)

  case mCurrency of
    Just currency 
      -> if newvalue > 0 then do
          let newCurrency = FileCurrencyValue currency newvalue

          result <- Import.liftIO $ defineCurrencyFileValue newCurrency

          case result of
            (Left err) -> sendResponseStatus status500 $ object ["message" .= err]
            (Right _) -> sendResponseStatus status200 $ object ["message" .= ("Valor alterado com sucesso!" :: Text)]

        else
          sendResponseStatus status400 $ object ["message" .= ("Valor inválido" :: Text)]
    _ -> sendResponseStatus status400 $ object ["message" .= ("Moeda inválida" :: Text)]


-------------------------------------------------------
-- | Constants
-------------------------------------------------------

pathFileCurrencies :: FilePath
pathFileCurrencies = "static/currencies.json"

listOfCurrencyLabels :: [(String, String)]
listOfCurrencyLabels = [
    ("USD", "United States Dollar"),
    ("BRL", "Brazilian Real"),
    ("EUR", "Euro"),
    ("CAD", "Canadian Dollar"),
    ("BTC", "Bitcoin")
  ]

-------------------------------------------------------
-- | Business Logic functions
-------------------------------------------------------

-- | Function that access "currencies.json" file and return their parsed data
readFileCurrencies :: IO [(String, Float)]
readFileCurrencies = do
  currencies <- P.readFile pathFileCurrencies

  let mJson = AS.decode $ BSU.fromString currencies :: Maybe (Map String String)

  case mJson of
    Just currencies' -> do
      let currenciesList = STS.toList currencies'

      return (P.map (second stringToFloat) currenciesList)
    _   -> error "Arquivo com formato incompatível"

-- | Function that is responsible to make request to the CoinIndex API
middlewareRequest :: IO BTC
middlewareRequest = do 
  file <- readFileCurrencies

  runReq defaultHttpConfig $ do
    response <- req REQ.GET (https "api.coindesk.com" /: "v1" /: "bpi" /: "currentprice" /: "BTC.json") NoReqBody jsonResponse mempty

    let (BTC time disclaimer (BPI bpi) ) = (REQ.responseBody response :: BTC)

    let mUsd = STS.lookup "USD" bpi

    case mUsd of
      Just usd -> do
        let placeholderBtc = T.pack (fromMaybe "" (getCurrencyLabelByCode "BTC"))

        let btc = fromMaybe (Currency "BTC" "1" placeholderBtc 1) (STS.lookup "BTC" bpi)

        let (Currency _ _ _ btcRate) = usd
        let baseList' = ("USD", usd) : ("BTC", btc) : P.map (calculateRateFloat btcRate) file

        let newList  = P.map (BF.first T.unpack) baseList'

        return (BTC time disclaimer (BPI (STS.fromList newList)))
      _ -> error "Moeda USD não retornado pela API"

-- | Function that change value of some currency item
defineCurrencyFileValue :: FileCurrencyValue -> IO (Either String Bool)
defineCurrencyFileValue (FileCurrencyValue code newvalue) = do
    currencies <- readFileCurrencies

    let mCurrency = find (\i -> fst i == show code) currencies

    let parseFile x = BSU.toString (AS.encode $ STS.fromList (parseValuesToString x))

    case mCurrency of
      Just _ -> do
        if newvalue < 0 then 
          return $ Left "Valor não pode ser negativo"
        else do
          let changeCurrencyValue (x, y) = (x, if x == show code then newvalue else y)

          let newFile = parseFile (P.map changeCurrencyValue currencies)

          writeFile pathFileCurrencies (Import.fromString newFile)

          return $ Right True
      _ -> do
        let newFile = parseFile (currencies ++ [(show code, newvalue)])

        writeFile pathFileCurrencies (Import.fromString newFile)

        return $ Right True


-------------------------------------------------------
-- | Helper functions
-------------------------------------------------------

calculateRateFloat :: Float -> (String, Float) -> (Text, Currency)
calculateRateFloat btcRate (code, rate) =  (textCode, Currency textCode rateText description rateFloat)
  where
    textCode = T.pack code
    rateFloat = rate * btcRate
    rateText = T.pack $ show rateFloat
    description = T.pack (fromMaybe "" (getCurrencyLabelByCode code))

floatToString :: Float -> String
floatToString = show

getCurrencyLabelByCode :: String -> Maybe String
getCurrencyLabelByCode code = fmap snd (find (\(code',_) -> code' == code) listOfCurrencyLabels)

parseValuesToString :: [(String, Float)] -> [(String, String)]
parseValuesToString = P.map (second floatToString)

stringToCurrencyValue ::  String -> Maybe AvailableCurrencies
stringToCurrencyValue = RT.readMaybe

stringToFloat :: String -> Float
stringToFloat = P.read
