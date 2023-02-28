{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module FrontEnd.Telegram.Data.SendPhoto where

import Data.Aeson
import Data.ByteString.Lazy

data ResponsePhoto = ResponsePhoto
  { okResponsePhoto :: Bool
  }
  deriving (Show)

decodeTopLevel :: ByteString -> Maybe ResponsePhoto
decodeTopLevel = decode

instance ToJSON ResponsePhoto where
  toJSON (ResponsePhoto okResponse') =
    object
      [ "ok" .= okResponse'
      ]

instance FromJSON ResponsePhoto where
  parseJSON (Object v) =
    ResponsePhoto
      <$> v
      .: "ok"
  parseJSON _ = error "parser error"
