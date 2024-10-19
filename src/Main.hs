{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Data.Text.Lazy (Text)

main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev

  -- Define seus roteadores e manipuladores aqui
  get "/nextDayRU" $ do
    text "RUZAO"