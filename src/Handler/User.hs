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
import qualified Yesod.Auth.Util.PasswordStore as PS
import qualified Text.Email.Validate  as Email

import Data.Text as T

import Types.UserLogin

-------------------------------------------------------
-- | Route handlers
-------------------------------------------------------

postUserLoginR :: Handler Value
postUserLoginR = do
    (UserLogin {..}) <- (requireCheckJsonBody :: Handler UserLogin)

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


-------------------------------------------------------
-- | Constants
-------------------------------------------------------

passwordLength :: Int
passwordLength = 6

-------------------------------------------------------
-- | Helper functions
-------------------------------------------------------

verifyPassword :: Text -> Text -> Bool
verifyPassword rawPassword dbPassword =
  PS.verifyPassword (encodeUtf8 rawPassword) $ encodeUtf8 dbPassword

verifyEmail :: Text -> Bool
verifyEmail email = Email.isValid $ encodeUtf8 email

stringToInt :: String -> Maybe Int
stringToInt = RT.readMaybe
