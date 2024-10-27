module EmailsHandler where

import System.IO
import Data.Char(toUpper)
import Data.List (intercalate)
import Data.List.Split (splitOn)

-- retorna emails na forma ["email1", "email2"]
getEmails :: FilePath -> IO String
getEmails path = do
    emails <- leArquivo path
    let formatedEmails = map(\s -> s++", ")emails
    let stringWithEmails = intercalate "" formatedEmails
    return stringWithEmails
--recebe um caminho de arquivo e retorna uma lista de emails
leArquivo :: FilePath -> IO [String]
leArquivo nomeArquivo = do
    inh <- openFile nomeArquivo ReadMode
    linhas <- lerLinhas inh
    hClose inh
    return linhas

-- le linha por linha do arquivo adicionando em uma lista os emails
lerLinhas :: Handle -> IO [String]
lerLinhas inh = do
    eof <- hIsEOF inh
    if eof
        then return []
        else do
            linha <- hGetLine inh
            outrasLinhas <- lerLinhas inh
            return (linha : outrasLinhas)

-- verifica se dentro do arquivo de emails já existe o email requisitado
emailExiste :: FilePath -> String -> IO Bool
emailExiste path email = do
    emails <- leArquivo path
    return (email `elem` emails)

-- se o email não existe adiciona ao final do arquivo, se existe retorna que já existe
adicionaEmail :: FilePath -> String -> IO (Either String ())
adicionaEmail path email = do 
    existe <- emailExiste path email
    if not existe
        then do
            arquivo <- openFile path AppendMode
            hPutStrLn arquivo email
            hClose arquivo
            return (Right ())
        else
            return (Left "Email já existe!")

-- se o email não existe retorna que o email não existe, se existe deleta ele do arquivo
deletaEmail :: FilePath -> String -> IO (Either String ())
deletaEmail path email = do
    existe <- emailExiste path email
    if existe
        then do
            conteudoDoArquivo <- leArquivo path
            -- passa uma lista que filtra todos os emails que NÃO são o que é para deletar
            reescreveArquivo path $ filter (\x -> x /= email)conteudoDoArquivo
            return (Right ())
        else
            return (Left "Email não existe!")

-- reescreve o arquivo com base na lista de Strings
reescreveArquivo :: FilePath -> [String] -> IO ()
reescreveArquivo path strings = do
    arquivo <- openFile path WriteMode
    mapM_ (hPutStrLn arquivo)strings
    hClose arquivo

-- retorna o email que realiza os envios em formato de lista de tupla (propriedade, valor)
getEmailSender :: FilePath ->  IO [(String, String)]
getEmailSender path = do
    propvalues <- leArquivo path
    return (map getPropAndValue propvalues)

getPropAndValue :: String -> (String, String)
getPropAndValue propvalue = (prop, value)
    where
        list = splitOn ":" propvalue
        prop = head list
        value = last list

-- Dada uma lista de tuplas de propriedade, valor e uma string que representa a propriedade que se 
-- deseja obter o valor, retorna esse valor.
getDesiredValue :: String -> [(String, String)] -> [Char]
getDesiredValue prop x = snd $ head $ filter(\(z,y) -> z == prop)x