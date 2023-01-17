--{-# LANGUAGE OverloadedStrings #-}

-- | The tekegram front-end
module Exemple.Telegram where

--import Servant.API
--import Servant.Client

-- import Network.HTTP.Client

-- import qualified Data.Text as T
--import qualified Data.Text.Read as T
--import qualified Data.Text.IO as TIO
--import qualified System.IO as SIO

-- import qualified FrontEnd.Telegram.Data.GetUpdate as Data
--import qualified FrontEnd.Telegram.API as API
-- import qualified EchoBot

import qualified FrontEnd.Telegram.Telegram as Telegram
import qualified ConfigBot

testUpdate :: IO ()
testUpdate = do
  conf <- ConfigBot.getTelegramConfig
  Telegram.testUpdate' conf
