{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Control.Monad (void)
import           Data.Semigroup ((<>))
import           Google.SendMail (sendMail')
import           Options.Applicative (Parser, strOption, long, metavar, help, helper, fullDesc, execParser, info)
import           Protolude

data Options = Options {
    credsFile  :: FilePath
  , svcAccUser :: Text
  , toEmail    :: Text
  , subject    :: Text
  , message    :: LText
  }

optionsParser :: Parser Options
optionsParser = Options
    <$> strOption
            (long "creds" <> metavar "CREDS_FILE" <> help "service account credentials file")
    <*> strOption
            (long "user" <> metavar "USER" <> help "email of user to impersonate")
    <*> strOption
            (long "to" <> metavar "TO_EMAIL" <> help "email address of recipient")
    <*> strOption
            (long "subject" <> metavar "SUBJECT" <> help "subject for the email being sent")
    <*> strOption
            (long "message" <> metavar "MESSAGE" <> help "plain text contents for the email being sent")

main :: IO ()
main = void (go =<< execParser opts)
    where
        go Options{..} = sendMail' credsFile svcAccUser toEmail subject message
        opts = info (optionsParser <**> helper) fullDesc
