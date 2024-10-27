{-# LANGUAGE OverloadedStrings #-}

module MailSender where

import Control.Monad
import Data.ByteString.Lazy (toStrict)
import Data.Text (pack)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Network.HaskellNet.Auth (AuthType(LOGIN))
import Network.HaskellNet.SMTP
import Network.HaskellNet.SMTP.SSL as SMTP
import Network.Mail.Mime
import qualified Data.Text.Lazy.Encoding as TE
import EmailsHandler

getMail :: String -> String -> String -> Mail
getMail remetente destinatario mensagem =
    mail
  where 
    htmlPart = Part
        { partType = "text/html"
        , partEncoding = Base64
        , partDisposition = DefaultDisposition 
        , partHeaders = [("Content-Language", "pt-br")]
        , partContent = PartContent $ TE.encodeUtf8 $ TL.pack mensagem -- Usar a mensagem fornecida
        }
    mail = Mail
        { mailFrom = Address Nothing (pack remetente)
        , mailTo = [Address Nothing (pack destinatario)]
        , mailCc = []
        , mailBcc = []
        , mailHeaders = []
        , mailParts = [[htmlPart]]
        } 

sendEMail :: String -> String -> IO ()
sendEMail destinatario mensagem = doSMTPSTARTTLS "smtp.gmail.com" $ \conn -> do
    propsAndValues <- getEmailSender "src/data/sender.txt"
    let sender = getDesiredValue "email" propsAndValues
    let pass = getDesiredValue "pass" propsAndValues
    authSucceed <- SMTP.authenticate LOGIN sender pass conn
    if authSucceed
    then do
        let mail = getMail sender destinatario mensagem
        sendMail mail conn
    else
        print "Nao foi possível enviar o e-mail"