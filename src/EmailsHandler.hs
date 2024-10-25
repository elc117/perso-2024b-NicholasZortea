module EmailsHandler where

import System.IO
import Data.Char(toUpper)

leArquivo :: FilePath -> IO [String]
leArquivo nomeArquivo = do
    inh <- openFile nomeArquivo ReadMode
    linhas <- lerLinhas inh
    hClose inh
    return linhas

lerLinhas :: Handle -> IO [String]
lerLinhas inh = do
    eof <- hIsEOF inh
    if eof
        then return []
        else do
            linha <- hGetLine inh
            outrasLinhas <- lerLinhas inh
            return (linha : outrasLinhas)

adicionaEmail :: FilePath -> String -> IO (Either String ())
adicionaEmail path email = do 
    emails <- leArquivo path
    if length (filter (\x -> x == email)emails) == 0
        then do
            arquivo <- openFile path AppendMode
            hPutStrLn arquivo email
            hClose arquivo
            return (Right ())
        else
            return (Left "Email já existe!")