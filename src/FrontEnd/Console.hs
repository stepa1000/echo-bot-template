{-# LANGUAGE OverloadedStrings #-}

-- | The console front-end is responsible for console I/O and
-- appropriate handling of other high-level bot interactions (menu
-- output etc).
module FrontEnd.Console
  ( run,
    Handle (..),
  )
where

import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.IO as TIO
import qualified EchoBot

newtype Handle = Handle
  { hBotHandle :: EchoBot.Handle IO T.Text
  }

run :: Handle -> IO ()
run _ = do
  TIO.putStrLn "Welcome to the echo-bot!"
  -- 1. Read a line from the console.
  -- 2. Send it to the bot, get its response and output it.
  -- 3. Go to 1.
  error "Not implemented"

runLoop :: Handle -> T.Text -> IO Bool
runLoop h t | t == "/exit" = return False
{- runLoop h t | (T.take 9 t) == "/setCount" = do
  let en = T.decimal $ T.drop 10 t
  case en of
    (Right (n,_)) -> do
      lr <- EchoBot.respond h $ EchoBot.SetRepetitionCountEvent n
      mapM (TIO.putStrLn . printResponse) lr -- !!!!!!!!!!
      return True
    (Left _) -> do
      TIO.putStrLn "Comand setCount is not right"
      return True
-}
runLoop h t = do
  lr <- EchoBot.respond (hBotHandle h) $ EchoBot.MessageEvent t
  mapM (TIO.putStrLn . printResponse) lr -- !!!!!!!!!!!
  return True

printResponse :: EchoBot.Response T.Text -> T.Text
printResponse = undefined

