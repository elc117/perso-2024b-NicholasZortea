{-# LANGUAGE DeriveGeneric #-}
module Refeicoes where

{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson (FromJSON(..), decode, withObject, (.:), Value(..), Key, eitherDecode)
import Data.Aeson.Key
import Data.Text (Text)
import GHC.Generics
import qualified Data.ByteString.Lazy as L
import System.IO (hPutStrLn, stderr)
import qualified Data.ByteString.Lazy.Char8 as B

data Refeicao = Refeicao
    { descricao :: Text
    , title     :: Text
    } deriving (Show, Generic)

-- Instância FromJSON para Refeicao
instance FromJSON Refeicao where
    parseJSON = withObject "Refeicao" $ \v -> do
        desc <- v .: fromString "descricao"
        tit <- v .: fromString "title"
        return $ Refeicao desc tit
        
getRefeicoes :: FilePath -> IO (Maybe [Refeicao])
getRefeicoes filePath = do
    -- Lê o conteúdo do arquivo
    jsonStr <- L.readFile filePath  -- Lê o arquivo como um ByteString
    let result = decode jsonStr :: Maybe [Refeicao]  -- Tenta decodificar o JSON
    case result of
        Just refeicoes -> return (Just refeicoes)
        Nothing -> do
            hPutStrLn stderr "Erro ao decodificar o JSON."  -- Usa stderr para erros
            return Nothing

getJsonString :: String
getJsonString = "{\n"++
  "\"descricao\": \"ovo,tomate,carne\",\n"++
  "\"title\": \"Café\"\n"++
    "}"
