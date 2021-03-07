{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.BTC where
 
import Data.Aeson

import Data.Map.Strict
import Data.Text as T

import Generics.Deriving.Base

-- | Data constructor that defines a Currency
data Currency = Currency 
            { 
              code        :: T.Text
            , rate        :: T.Text
            , description :: T.Text
            , rateFloat   :: Float } deriving Show

-- | Data constructor of response time given by CoinIndex API
data Time = Time
            {
              updated     ::  T.Text
            , updatedISO  :: T.Text
            , updatedUk   :: T.Text
            } deriving (Show, Generic)

-- | Data constructor that defines the Bitcoin Price Index (BPI)
data BPI = BPI (Map String Currency) deriving (Show, Generic)

-- | Data constructor of CoinIndex API response
data BTC = BTC
            {
                time        :: Time
              , disclaimer  :: T.Text
              , bpi         :: BPI
            } deriving (Show, Generic)

instance FromJSON Currency where
  parseJSON = withObject "Currency" $ \v -> 
    Currency  <$> v .: "code"
              <*> v .: "rate"
              <*> v .: "description"
              <*> v .: "rate_float"

instance ToJSON Currency where
  toJSON (Currency code' rate' description' rateFloat') =
    object [
        "code"        .= code'
      , "rate"        .= rate'
      , "description" .= description'
      , "rate_float"  .= rateFloat'
    ]

instance FromJSON Time where
  parseJSON = withObject "Time" $ \v -> 
    Time  <$> v .: "updated"
          <*> v .: "updatedISO"
          <*> v .: "updateduk"

instance ToJSON Time where
  toJSON (Time updated' updatedISO' updatedUk') =
    object [
        "updated"     .= updated'
      , "updatedISO"  .= updatedISO'
      , "updateduk"   .= updatedUk'
    ]

instance FromJSON BPI
instance ToJSON BPI

instance FromJSON BTC where
  parseJSON = withObject "BTC" $ \v -> 
    BTC <$> v .: "time"
        <*> v .: "disclaimer"
        <*> v .: "bpi"

instance ToJSON BTC where
  toJSON (BTC time' disclaimer' bpi') =
    object [
        "time" .= time'
      , "disclaimer" .= disclaimer'
      , "bpi" .= bpi'
    ]