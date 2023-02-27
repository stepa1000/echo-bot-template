{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module FrontEnd.Telegram.API where

import Data.Proxy
import Data.Text as T
import FrontEnd.Telegram.Data.GetUpdate as GU
import FrontEnd.Telegram.Data.PollMessage as PM
import FrontEnd.Telegram.Data.SendMessage as SM
import FrontEnd.Telegram.Data.SendPhoto as SP
import Servant.API
import Servant.Client

newtype ListText = ListText {unListText :: [T.Text]}

instance ToHttpApiData ListText where
  toUrlPiece (ListText l) = "[" `T.append` (f l `T.append` "]")
    where
      f [x] = "\"" `T.append` (x `T.append` "\"")
      f (x : xs) = "\"" `T.append` x `T.append` "\"" `T.append` ("," `T.append` f xs)
      f _ = ""

type TelegramAPI =
  "getUpdates" :> QueryParam "offset" Int :> Get '[JSON] GU.ResponseUpdate
    :<|> "sendMessage"
      :> QueryParam "chat_id" Int
      :> QueryParam "text" T.Text
      :> Get '[JSON] SM.ResponseSendMessage
    :<|> "sendPhoto"
      :> QueryParam "chat_id" Int
      :> QueryParam "photo" T.Text
      :> Get '[JSON] SP.ResponsePhoto
    :<|> "sendPoll"
      :> QueryParam "chat_id" Int
      :> QueryParam "question" T.Text
      :> QueryParam "options" ListText
      :> Get '[JSON] PM.ResponsePoll

telegramAPI :: Proxy TelegramAPI
telegramAPI = Proxy

getUpdates :: Maybe Int -> ClientM GU.ResponseUpdate
sendMessage :: Maybe Int -> Maybe T.Text -> ClientM SM.ResponseSendMessage
sendPhoto :: Maybe Int -> Maybe T.Text -> ClientM SP.ResponsePhoto
sendPoll :: Maybe Int -> Maybe T.Text -> Maybe ListText -> ClientM PM.ResponsePoll
getUpdates :<|> sendMessage :<|> sendPhoto :<|> sendPoll = client telegramAPI
