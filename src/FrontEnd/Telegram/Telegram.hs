{-# LANGUAGE OverloadedStrings #-}

-- | The tekegram front-end
module FrontEnd.Telegram.Telegram where

--import Servant.API
-- import Servant.Client

import Network.HTTP.Client

import qualified Data.Text as T
--import qualified Data.Text.Read as T
--import qualified Data.Text.IO as TIO
import qualified System.IO as SIO

-- import qualified FrontEnd.Telegram.Data.GetUpdate as Data
import qualified FrontEnd.Telegram.API as API
-- import qualified EchoBot

data Config = Config
  { confBotToken :: T.Text -- confManager :: 
  } deriving Show
{-
clientEnvDefault :: Config -> IO ClientEnv
clientEnvDefault conf = do 
  m <- newManager defaultManagerSettings
  url <- parseBaseUrl $ "https://api.telegram.org/bot" ++ (T.unpack $ confBotToken conf)
  return $ mkClientEnv m url
-}
testUpdate' :: Config -> IO () -- (Either ClientError Data.Welcome10)
testUpdate' conf = do
  m <- newManager defaultManagerSettings
  e <- API.getUpdate 
    ("https://api.telegram.org/bot" ++ (T.unpack $ confBotToken conf) ) 
    m 
    1
{- runClientM (do
    API.getUpdates (Just 1)
    ) m
-}
  SIO.print e 
