{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Data.Text.Lazy (Text, pack)
import Network.HTTP.Types.Header (hContentType)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import System.FilePath ((</>))  -- Para construção de caminhos de arquivo
import System.Directory (getCurrentDirectory)
import qualified Data.Aeson as Aeson
import Network.HTTP.Types.Status (conflict409)
import Network.HTTP.Client (defaultManagerSettings, Manager, httpLbs, parseRequest, responseBody)
import Network.HTTP.Client.TLS (getGlobalManager)
import Data.Aeson (Value,decode, encode)
import Data.Time (getCurrentTime, utctDay, addDays, diffUTCTime, UTCTime(..))
import Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)
import Data.Maybe (fromMaybe)
import EmailsHandler
import MailSender
import Pages
import Text.Printf(printf)
import qualified Data.Text as Txt
import Refeicoes

-- Função principal que primeiro captura os cookies e depois faz outra requisição usando eles

getBody :: String -> IO (Maybe Value)  -- Alterado para retornar um Maybe Value (JSON)
getBody url = do
    manager <- getGlobalManager
    request <- parseRequest url
    response <- httpLbs request manager
    let body = responseBody response
    return $ decode body  -- Tenta decodificar o corpo como JSON

getInicio :: IO String
getInicio = do
    currentTime <- getCurrentTime
    let today = utctDay currentTime
        midnightNextDay = UTCTime (addDays 2 today) 0  -- Próxima meia-noite
        midnightTimestamp = utcTimeToPOSIXSeconds midnightNextDay  -- Converte para timestamp UNIX
    return (printf "%.0f" (realToFrac midnightTimestamp :: Double)) -- Formata o timestamp como String

--Função que faz a requisição e salva o JSON em um arquivo
saveCardapio :: IO (Maybe Value)
saveCardapio = do
    inicio <- getInicio  -- Chama getInicio para obter o timestamp
    putStrLn $ "timestamp: " ++ inicio
    let url = "https://portal.ufsm.br/ru/publico/buscarCardapio.json?inicio=" ++ inicio ++ "&fim=" ++ inicio ++ "&idRestaurante=1&tiposRefeicao%5B%5D=1&tiposRefeicao%5B%5D=2&tiposRefeicao%5B%5D=3"
    
    putStrLn $ "Requisição para a URL: " ++ url
    body <- getBody url
    case body of
        Just jsonBody -> do
            currentDir <- getCurrentDirectory
            let filePath = currentDir </> "cardapio.json"
            L.writeFile filePath (encode jsonBody)
            putStrLn $ "JSON salvo em: " ++ filePath
            return (Just jsonBody)
        Nothing -> do
            putStrLn "Erro ao processar JSON"
            return Nothing


main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev
    
    post "/addEmail/:e" $ do
        email <- param "e" :: ActionM String
        resultado <- liftIO (adicionaEmail "src/data/emails.txt" email)
        case resultado of
            Right () -> text "Email adicionado com sucesso!"
            Left msg -> do
                status conflict409
                text (pack msg)

    delete "/deleteEmail/:e" $ do
        email <- param "e" :: ActionM String
        resultado <- liftIO (deletaEmail "src/data/emails.txt" email)
        case resultado of
            Right () -> text "Email deletado com sucesso!"
            Left msg -> do
                status conflict409
                text (pack msg)
    
    get "/getEmails" $ do
        emails <- liftIO (getEmails "src/data/emails.txt")
        text (pack emails)
    
    get "/" $ do
        liftIO saveCardapio
        pagina <- liftIO $ getMainPage "src/data/emails.txt" "cardapio.json"
        html (pack $ pagina)

    get "/css" $ do
        file "src/style.css"
    
    get "/javascript" $ do
        file "src/script.js"

    get "/sendMailToAll" $ do 
        allMails <- liftIO $ leArquivo "src/data/emails.txt"
        cardapio <- liftIO $ getDivCardapio "cardapio.json"
        liftIO $ mapM_(\x -> sendEMail x cardapio) allMails
        text "E-mails enviados!"

    -- get "/testeJsonParse" $ do
       -- let string = getJsonString
    --    liftIO $ putStrLn $ "JSON String: " ++ string  -- Para verificar o JSON

--        refeicoes <- liftIO $ getRefeicoes "cardapio.json"  -- Chamada de IO agora
   --     let refeicoes' = fromMaybe [] refeicoes
     --   liftIO $ print refeicoes'  -- Imprime a lista de refeicoes

       -- let tupla = getRefeicaoFormatoCorreto refeicoes' -- [(titulo, [alimentos])]
       -- liftIO $ putStrLn $ "Tuplas: " 
       -- mapM_ (\(titulo, alimentos) -> do
       --     liftIO $ putStrLn titulo  -- imprime o título
        --    printStrings alimentos) tupla

--        let cafe = filter (\r -> title r == "Café") refeicoes'
  --      liftIO $ print (length cafe)  -- Imprime a quantidade filtrada

--        let descricoes = unlines $ map(Txt.unpack . descricao) cafe
  --      text $ pack descricoes