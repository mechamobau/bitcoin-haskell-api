{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards   #-}

module Handler.User where

import Import
import Text.Read as RT
import Data.Text as T
import qualified Yesod.Auth.Util.PasswordStore as PS
import qualified Data.ByteString.Char8 as BC
import qualified Text.Email.Validate  as Email
import           Data.CaseInsensitive as CI

data Login = Login
                { loginEmail    :: T.Text
                , loginPassword :: T.Text
                } deriving (Show)

instance FromJSON Login where
    parseJSON (Object v) =
        Login   <$> v .: "email"
                <*> v .: "password"

instance ToJSON Login where
    toJSON (Login email password) =
        object [
            "email"     .= email
        ,   "password"  .= password
        ]

passwordLength :: Int
passwordLength = 6

stringToInt :: String -> Maybe Int
stringToInt = RT.readMaybe

postUserLoginR :: Handler Value
postUserLoginR = do
    (Login {..}) <- (requireCheckJsonBody :: Handler Login)

    mUser <- runDB $ getBy $ UniqueUserEmail loginEmail

    case mUser of
        Just (Entity userId (User {..})) | validEmail && validPwd-> do
            token <- userIdToToken userId
            return $ object
                    [
                        "token"       .= token
                    ]
            where
                validPwdLength = maybe False (\password' -> Import.length (show password')==passwordLength) (stringToInt $ T.unpack loginPassword)
                validPwd = validPwdLength && (verifyPassword loginPassword userPassword)
                validEmail = verifyEmail userEmail

        _ -> invalidArgs ["wrong password or email"]

verifyPassword :: Text -> Text -> Bool
verifyPassword rawPassword dbPassword =
  PS.verifyPassword (encodeUtf8 rawPassword) $ encodeUtf8 dbPassword

verifyEmail :: Text -> Bool
verifyEmail email = Email.isValid $ encodeUtf8 email
