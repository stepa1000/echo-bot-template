{-# Language TypeOperators ,
DataKinds 
#-}

module FrontEnd.Telegram.API where

import Servant.API
import Servant.Client

import Data.Proxy

import FrontEnd.Telegram.Data.GetUpdate as GU
import FrontEnd.Telegram.Data.SendMessage as SM

type TelegramAPI = "getUpdates" :> QueryParam "offset" Int :> Get '[JSON] Welcome10
              :<|> "sendMessage" :> QueryParam "chat_id" Int :> Get '[JSON] Welcome2
{- (QueryParam "chat_id" Int 
                                 :<|> QueryParam "chat_id" String )
-}
telegramAPI :: Proxy TelegramAPI
telegramAPI = Proxy

getUpdates :: Maybe Int -> ClientM Welcome10
sendMessage :: Maybe Int -> ClientM Welcome2

getUpdates :<|> sendMessage = client telegramAPI
