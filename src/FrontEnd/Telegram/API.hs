{-# Language TypeOperators ,
DataKinds ,
FlexibleInstances,
OverloadedStrings
#-}

module FrontEnd.Telegram.API where

import Servant.API
import Servant.Client

import Data.Text as T
import Data.Proxy
-- import Data.Vector

import FrontEnd.Telegram.Data.GetUpdate as GU
import FrontEnd.Telegram.Data.SendMessage as SM
import FrontEnd.Telegram.Data.PollMessage as PM
import FrontEnd.Telegram.Data.SendPhoto as SP

newtype ListText = ListText {unListText :: [T.Text]}

instance ToHttpApiData ListText where
  toUrlPiece (ListText l) = "[" `T.append` ( f l `T.append` "]")
    where
      f [x] = "\"" `T.append` (x `T.append` "\"" )
      f (x:xs) = "\"" `T.append` x `T.append` "\""  `T.append` ("," `T.append` f xs )
      f _ = ""

type TelegramAPI = "getUpdates" :> QueryParam "offset" Int :> Get '[JSON] GU.Welcome10
              :<|> "sendMessage" :> QueryParam "chat_id" Int 
                                 :> QueryParam "text" T.Text 
                                 -- :> QueryParam' '[Optional,Strict,JSON] "" (Vector GU.Entity)
                                 :> Get '[JSON] SM.Welcome2
              :<|> "sendPhoto" :> QueryParam "chat_id" Int
                               :> QueryParam "photo" T.Text
                               :> Get '[JSON] SP.Welcome 
              :<|> "sendPoll" :> QueryParam "chat_id" Int
                              :> QueryParam "question" T.Text
                              :> QueryParam "options" ListText
                              :> Get '[JSON] PM.Welcome9
{- (QueryParam "chat_id" Int 
                                 :<|> QueryParam "chat_id" String )
-}
telegramAPI :: Proxy TelegramAPI
telegramAPI = Proxy

getUpdates :: Maybe Int -> ClientM GU.Welcome10
sendMessage :: Maybe Int -> Maybe T.Text -> ClientM SM.Welcome2
sendPhoto :: Maybe Int -> Maybe T.Text -> ClientM SP.Welcome
sendPoll :: Maybe Int -> Maybe T.Text -> Maybe ListText -> ClientM PM.Welcome9

getUpdates :<|> sendMessage :<|> sendPhoto :<|> sendPoll = client telegramAPI
