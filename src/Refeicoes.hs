{-# LANGUAGE DeriveGeneric #-}
module Refeicoes where

{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson (FromJSON(..), decode, withObject, (.:), Value(..), Key, eitherDecode)
import Data.Aeson.Key
import Data.Text (Text,unpack)
import GHC.Generics
import qualified Data.ByteString.Lazy as L
import System.IO (hPutStrLn, stderr)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List.Split (splitOn)

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

getRefeicaoAsTupla :: Refeicao -> (String, String)
getRefeicaoAsTupla refeicao = (unpack(title refeicao), unpack(descricao refeicao))

getDescAsList :: String -> [String]
getDescAsList descricao = list
    where 
        list = splitOn "<br/>" descricao

getRefeicaoFormatoCorreto :: [Refeicao] -> [(String, [String])]
getRefeicaoFormatoCorreto refeicoes = tuplaCorreta
    where 
        tuplas = map getRefeicaoAsTupla refeicoes
        tuplaCorreta = map (\(x, y) -> (x, getDescAsList y))tuplas