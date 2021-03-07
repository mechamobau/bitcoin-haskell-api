{-# LANGUAGE DeriveGeneric #-}
module Types.Currency where

import Data.Aeson

import Generics.Deriving.Base

import Data.Text (Text)

-- | Data Constructor that defines all available Currency keys
data AvailableCurrencies = EUR | BRL | CAD deriving (Show, Generic)

-- | Data Constructor that defines a new Currency value
data NewCurrencyValue = NewCurrencyValue {
                        currencyCode :: Text,
                        currencyValue :: Float
                      } deriving (Show, Generic)

-- | Data Constructor that lists all available Currencies
data FileCurrencyValue = FileCurrencyValue {
                        fileCurrencyCode :: AvailableCurrencies,
                        fileCurrencyValue :: Float
                      } deriving (Show, Generic)

instance Read AvailableCurrencies where
    readsPrec _ value = 
        tryParse [("EUR", EUR), ("BRL", BRL), ("CAD", CAD)]
        where tryParse [] = []
              tryParse ((attempt, result):xs) =
                      if (take (length attempt) value) == attempt   
                         then [(result, drop (length attempt) value)]
                         else tryParse xs

instance FromJSON AvailableCurrencies
instance ToJSON AvailableCurrencies

instance FromJSON NewCurrencyValue
instance ToJSON NewCurrencyValue

instance FromJSON FileCurrencyValue
instance ToJSON FileCurrencyValue
