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
  "getUpdates" :> QueryParam "offset" Int :> Get '[JSON] GU.WelcomeUpdate
    :<|> "sendMessage" :> QueryParam "chat_id" Int
      :> QueryParam "text" T.Text
      :> Get '[JSON] SM.WelcomeSendMessage
    :<|> "sendPhoto" :> QueryParam "chat_id" Int
      :> QueryParam "photo" T.Text
      :> Get '[JSON] SP.WelcomePhoto
    :<|> "sendPoll" :> QueryParam "chat_id" Int
      :> QueryParam "question" T.Text
      :> QueryParam "options" ListText
      :> Get '[JSON] PM.WelcomePoll

telegramAPI :: Proxy TelegramAPI
telegramAPI = Proxy

getUpdates :: Maybe Int -> ClientM GU.WelcomeUpdate

sendMessage :: Maybe Int -> Maybe T.Text -> ClientM SM.WelcomeSendMessage

sendPhoto :: Maybe Int -> Maybe T.Text -> ClientM SP.WelcomePhoto

sendPoll :: Maybe Int -> Maybe T.Text -> Maybe ListText -> ClientM PM.WelcomePoll
getUpdates :<|> sendMessage :<|> sendPhoto :<|> sendPoll = client telegramAPI
