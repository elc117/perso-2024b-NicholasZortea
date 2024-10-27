module Pages where

import EmailsHandler
import Refeicoes
import Data.Maybe (fromMaybe)

getEmailsAsUnorderedList :: FilePath -> IO String
getEmailsAsUnorderedList emailsPath = do
    emails <- leArquivo emailsPath
    let liEmails = ["<li>"++x++"</li>\n" | x <- emails]
    return ("<ul>\n" ++ concat liEmails ++ "</ul>\n")

--                 caminho do cardapio
getDivCardapio :: FilePath -> IO String
getDivCardapio cardapio = do
    refeicoes <- getRefeicoes cardapio  -- Use a operação IO
    let refeicoes' = fromMaybe [] refeicoes
    let tuplas = map getRefeicaoAsTupla refeicoes'

    -- Filtrando as refeições
    let cafe = snd $ head $ filter (\(titulo, descricao) -> titulo == "Café") tuplas
    let almoco = snd $ head $ filter (\(titulo, descricao) -> titulo == "Almoço") tuplas
    let jantar = snd $ head $ filter (\(titulo, descricao) -> titulo == "Jantar") tuplas

    return (
            "<div class=\"margin\"> \n"++
                "<h4>Café:</h4>\n"++
                cafe ++
                "</br><h4>Almoço:</h4>\n"++
                almoco ++
                "</br><h4>Jantar:</h4>\n"++
                jantar ++
            "</div>\n"
            )

getMainPage :: FilePath -> FilePath -> IO String
getMainPage emails cardapio = do
    emailList <- getEmailsAsUnorderedList emails
    cardapio <- getDivCardapio cardapio
    return $ "<html>\n" ++
            "   <head>\n" ++
            "       <title>Cadastro de emails</title>\n"++
            "       <link rel=\"stylesheet\" type=\"text/css\" href=\"/css\">\n" ++
            "       <script type=\"text/javascript\" src=\"/javascript\"></script>\n" ++
            "   </head>\n" ++ 
            "    <body>\n" ++
            "       <div class=\"menuAndMails\">\n"++
                        cardapio ++
            "           <div class=\"margin\">\n" ++

            "              <div>\n" ++
            "              <h3>Lista de E-mails cadastrados: \n</h3>"++
                            emailList ++
            "              </div>\n" ++

            "              <div>\n" ++
            "                   <form onsubmit=\"return false\">\n"++
            "                       <label for=\"email\">Email:</label><br>\n" ++
            "                       <input type=\"email\" id=\"email\" name=\"email\"><br>\n" ++
            "                      <button type=\"button\" onclick=\"addEmail()\">Adiciona</button>\n"++
            "                      <button type=\"button\" onclick=\"deleteEmail()\">Deleta</button>\n"++
            "                   </form>\n"++
            "              </div>\n" ++
            "           </div>\n" ++
            "       </div>\n"++
                    "<button class=\"sendBtn\" type=\"button\" onclick=\"sendAllMails()\">Enviar cardápio para todos os emails</button>\n"++
            "    </body>\n" ++
            "</html>"
