{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module FrontEnd.Telegram.Data.SendPhoto where

import Data.Aeson
import Data.ByteString.Lazy

data WelcomePhoto
  = WelcomePhoto
      { okWelcomePhoto :: Bool
      }
  deriving (Show)

decodeTopLevel :: ByteString -> Maybe WelcomePhoto
decodeTopLevel = decode

instance ToJSON WelcomePhoto where
  toJSON (WelcomePhoto okWelcome') =
    object
      [ "ok" .= okWelcome'
      ]

instance FromJSON WelcomePhoto where
  parseJSON (Object v) =
    WelcomePhoto
      <$> v .: "ok"
  parseJSON _ = error "parser error"
