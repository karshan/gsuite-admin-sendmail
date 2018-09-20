{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module Google.SendMail where

import           Control.Lens ((.~))
import           Network.Google ((!))
import qualified Network.Google as Google
import qualified Network.Google.Auth as Google
import qualified Network.Google.Gmail as Google
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.Mail.Mime (Address(..), Mail, renderMail', simpleMail')

import qualified Data.ByteString.Base64.URL as B64Url
import           Protolude

sendMail' :: (Exception e, StringConv a Text, StringConv b LText) => FilePath -> a -> a -> a -> b -> IO (Either e Google.Message)
sendMail' svcAccKey svcAccUser to subject msg =
    sendMail svcAccKey svcAccUser $ simpleMail' (Address Nothing (toS to)) (Address Nothing (toS svcAccUser)) (toS subject) (toS msg)

sendMail :: (StringConv a Text, Exception e) => FilePath -> a -> Mail -> IO (Either e Google.Message)
sendMail svcAccKey svcAccUser mail = try $ do
    rawMail <- B64Url.encode . toS <$> renderMail' mail
    mgr <- newManager tlsManagerSettings
    creds <- Google.serviceAccountUser (Just $ toS svcAccUser) <$> Google.fromFilePath svcAccKey
    env <- (Google.envScopes .~ Google.gmailSendScope) <$> Google.newEnvWith creds (\_ _ -> pure ()) mgr
    Google.runResourceT . Google.runGoogle env $
        Google.send (Google.usersMessagesSend (Google.message & Google.mRaw .~ Just rawMail) & Google.umsUserId .~ (toS svcAccUser))
