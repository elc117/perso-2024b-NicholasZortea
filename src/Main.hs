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
import Data.Aeson (Value,decode)
import Data.Time.Clock (getCurrentTime, utctDayTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Maybe (fromMaybe)
import EmailsHandler
import MailSender
import Pages
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
    currentTime <- round <$> getPOSIXTime  
    return (show currentTime)      
getFim :: IO String
getFim = do
    inicio <- getInicio
    let inicioMillis = read inicio :: Int  -- Converte o resultado de volta para Int
    return (show (inicioMillis + 86400)) -- Adiciona 86400000 milissegundos (1 dia)

main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev

    get "/cardapio" $ do
        inicio <- liftIO getInicio  -- Chama getInicio
        fim <- liftIO getFim        -- Chama getFim
        let url = "https://portal.ufsm.br/ru/publico/buscarCardapio.json?inicio=1729998000&fim=1729998000&idRestaurante=1&tiposRefeicao%5B%5D=1&tiposRefeicao%5B%5D=2&tiposRefeicao%5B%5D=3"
        
         -- Imprime a URL no console
        liftIO $ putStrLn $ "Requisição para a URL: " ++ url


        body <- liftIO $ getBody url
        case body of
            Just jsonBody -> do
                -- Obtém o diretório atual e cria o caminho do arquivo
                currentDir <- liftIO getCurrentDirectory
                let filePath = currentDir </> "cardapio.json"
                
                -- Converte jsonBody (Value) para ByteString e salva no arquivo
                liftIO $ L.writeFile filePath (Aeson.encode jsonBody)
                liftIO $ putStrLn $ "JSON salvo em: " ++ filePath
                
                json jsonBody  -- Retorna a resposta como JSON
            Nothing -> text "Erro ao processar JSON"  -- Caso o parsing falhe
    
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

    get "/TesteEmail" $ do
        liftIO $ sendEMail "giordanacamilla@gmail.com" "<!DOCTYPE html>\
                  \ <html lang=\"pt-BR\">\
                  \ <head>\
                  \ <meta charset=\"UTF-8\">\
                  \ <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\
                  \ <title>Teste de E-mail</title>\
                  \ </head>\
                  \ <body>\
                  \ <h1>Teste de E-mail</h1>\
                  \ <p>Olá, este é um teste de envio de e-mail usando Haskell!</p>\
                  \ <p>Se você está vendo isso, significa que o envio foi bem-sucedido!</p>\
                  \ </body>\
                  \ </html>"
        text "E-mail enviado com sucesso!"
    
    get "/" $ do
        pagina <- liftIO $ getMainPage "src/data/emails.txt"
        html (pack $ pagina)

    get "/css" $ do
        file "src/style.css"
    
    get "/javascript" $ do
        file "src/script.js"

    get "/sendMailToAll" $ do 
        allMails <- liftIO $ leArquivo "src/data/emails.txt"
        liftIO $ mapM_(\x -> sendEMail x "teste") allMails
        text "E-mails enviados!"

    get "/testeJsonParse" $ do
        let string = getJsonString
        liftIO $ putStrLn $ "JSON String: " ++ string  -- Para verificar o JSON

        refeicoes <- liftIO $ getRefeicoes "src/cardapio.json"  -- Chamada de IO agora
        let refeicoes' = fromMaybe [] refeicoes
        liftIO $ print refeicoes'  -- Imprime a lista de refeicoes

        let cafe = filter (\r -> title r == "Café") refeicoes'
        liftIO $ print (length cafe)  -- Imprime a quantidade filtrada

        let descricoes = unlines $ map(Txt.unpack . descricao) cafe
        text $ pack descricoes