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
run h = do
  TIO.putStrLn "Welcome to the echo-bot!"
  tn <- TIO.getLine
  runLoop h tn
  -- 1. Read a line from the console.
  -- 2. Send it to the bot, get its response and output it.
  -- 3. Go to 1.
  -- error "Not implemented"

runLoop :: Handle -> T.Text -> IO ()
runLoop h t = do
  b <- runLoop' h t
  if b then do
    tn <- TIO.getLine
    runLoop h tn
    else return ()

runLoop' :: Handle -> T.Text -> IO Bool
runLoop' _ t | t == "/exit" = return False
runLoop' h t = do
  lr <- EchoBot.respond (hBotHandle h) $ EchoBot.MessageEvent t
  case lr of
    ((EchoBot.MenuResponse t2 lm):_) -> do
        TIO.putStrLn t2
        _ <- (TIO.putStrLn . printMenu) lm
        tn <- TIO.getLine
        let conf = EchoBot.hConfig (hBotHandle h)
        lr2 <- EchoBot.respond (hBotHandle h) $ 
          EchoBot.SetRepetitionCountEvent $ 
          either (const $ EchoBot.confRepetitionCount conf) id $ 
          fmap fst $
          T.decimal tn
        _ <- mapM (TIO.putStrLn . printResponse) lr2 -- !!!!!!!!!!!
        return True
    ls -> do
       _ <- mapM (TIO.putStrLn . printResponse) ls
       return True

printResponse :: EchoBot.Response T.Text -> T.Text
printResponse (EchoBot.MessageResponse t) = t
printResponse _ = ""

printMenu :: [(EchoBot.RepetitionCount, EchoBot.Event T.Text)] -> T.Text
printMenu = foldl1 (\t1 t2-> t1 `T.append` "\n" `T.append` t2) . fmap (T.pack . show)
