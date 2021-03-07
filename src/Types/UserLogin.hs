{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.UserLogin where

import Data.Aeson

import Data.Text as T

data UserLogin = UserLogin
                { loginEmail    :: T.Text
                , loginPassword :: T.Text
                } deriving (Show)

instance FromJSON UserLogin where
    parseJSON = withObject "UserLogin" $ \v -> 
        UserLogin   <$> v .: "email"
                    <*> v .: "password"

instance ToJSON UserLogin where
    toJSON (UserLogin email password) =
        object [
            "email"     .= email
        ,   "password"  .= password
        ]