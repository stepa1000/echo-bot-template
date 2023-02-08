{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module FrontEnd.Telegram.Data.SendMessage where

import Data.Aeson
import Data.ByteString.Lazy

data WelcomeSendMessage
  = WelcomeSendMessage
      { okWelcomeSendMesssage :: Bool
      }
  deriving (Show)

decodeTopLevel :: ByteString -> Maybe WelcomeSendMessage
decodeTopLevel = decode

instance ToJSON WelcomeSendMessage where
  toJSON (WelcomeSendMessage okWelcome2') =
    object
      [ "ok" .= okWelcome2'
      ]

instance FromJSON WelcomeSendMessage where
  parseJSON (Object v) =
    WelcomeSendMessage
      <$> v .: "ok"
  parseJSON _ = error "parser"
