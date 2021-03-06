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
import Prelude                      as P        (read, readFile, map)
import Data.Aeson                   as AS       (decode)
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


getCryptoR :: Handler Value
getCryptoR = do
  (BTC time disclaimer bpi) <- Import.liftIO middlewareRequest

  print currencies_json

  returnJson $ object [
        "time" .= time
      , "disclaimer" .= disclaimer
      , "bpi" .= bpi
    ]

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

calculateRateFloat :: Float -> (String, Float) -> (Text, Currency)
calculateRateFloat btcRate (code, rate) =  (textCode, Currency textCode rateText description rateFloat)
  where
    textCode = T.pack code
    rateFloat = rate * btcRate
    rateText = T.pack $ show rateFloat
    description = T.pack (fromMaybe "" (getCurrencyLabelByCode code))

getCurrencyLabelByCode :: String -> Maybe String
getCurrencyLabelByCode code = fmap snd (find (\(code',_) -> code' == code) listOfCurrencyLabels)

stringToFloat :: String -> Float
stringToFloat = P.read

floatToString :: Float -> String
floatToString = show

readFileCurrencies :: IO [(String, Float)]
readFileCurrencies = do
  currencies <- P.readFile pathFileCurrencies

  let mJson = AS.decode $ BSU.fromString currencies :: Maybe (Map String String)

  case mJson of
    Just currencies' -> do
      let currenciesList = STS.toList currencies'

      return (P.map (second stringToFloat) currenciesList)
    _   -> error "Arquivo com formato incompatível"


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
