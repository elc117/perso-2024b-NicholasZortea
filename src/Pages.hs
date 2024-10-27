module Pages where

import EmailsHandler

getEmailsAsUnorderedList :: FilePath -> IO String
getEmailsAsUnorderedList emailsPath = do
    emails <- leArquivo emailsPath
    let liEmails = ["<li>"++x++"</li>\n" | x <- emails]
    return ("<ul>\n" ++ concat liEmails ++ "</ul>\n")

getMainPage :: FilePath -> IO String
getMainPage path = do
    emailList <- getEmailsAsUnorderedList path
    return $ "<html>\n" ++
            "   <head>\n" ++
            "       <title>Cadastro de emails</title>\n"++
            "       <link rel=\"stylesheet\" type=\"text/css\" href=\"/css\">\n" ++
            "       <script type=\"text/javascript\" src=\"/javascript\"></script>\n" ++
            "   </head>\n" ++ 
            "    <body>\n" ++
            "        <div>\n" ++

            "           <div>\n" ++
            "           <h3>Lista de E-mails cadastrados: \n</h3>"++
                        emailList ++
            "           </div>\n" ++

            "           <div>\n" ++
            "                <form onsubmit=\"return false\">\n"++
            "                    <label for=\"email\">Email:</label><br>\n" ++
            "                    <input type=\"email\" id=\"email\" name=\"email\"><br>\n" ++
            "                   <button type=\"button\" onclick=\"addEmail()\">Adiciona</button>\n"++
            "                   <button type=\"button\" onclick=\"deleteEmail()\">Deleta</button>\n"++
            "                </form>\n"++
            
            "           </div>\n" ++

            "        </div>\n" ++
            "    </body>\n" ++
            "</html>"
