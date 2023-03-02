{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module FrontEnd.Telegram.Data.SendMessage where

import Data.Aeson
import Data.ByteString.Lazy

newtype ResponseSendMessage = ResponseSendMessage
  { okResponseSendMesssage :: Bool
  }
  deriving (Show)

decodeTopLevel :: ByteString -> Maybe ResponseSendMessage
decodeTopLevel = decode

instance ToJSON ResponseSendMessage where
  toJSON (ResponseSendMessage okResponse') =
    object
      [ "ok" .= okResponse'
      ]

instance FromJSON ResponseSendMessage where
  parseJSON (Object v) =
    ResponseSendMessage
      <$> v
      .: "ok"
  parseJSON _ = error "parser"
