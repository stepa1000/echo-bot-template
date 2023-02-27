{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module FrontEnd.Telegram.Data.PollMessage where

import Data.Aeson
import Data.ByteString.Lazy
import Data.Text

data ResponsePoll = ResponsePoll
  { okResponsePoll :: Bool,
    resultResponsePoll :: ResultClass
  }
  deriving (Show)

data ResultClass = ResultClass
  { pollResultClass :: Poll
  }
  deriving (Show)

data Poll = Poll
  { pollIDPoll :: Text
  }
  deriving (Show)

decodeTopLevel :: ByteString -> Maybe ResponsePoll
decodeTopLevel = decode

instance ToJSON ResponsePoll where
  toJSON (ResponsePoll okResponse' resultResponse') =
    object
      [ "ok" .= okResponse',
        "result" .= resultResponse'
      ]

instance FromJSON ResponsePoll where
  parseJSON (Object v) =
    ResponsePoll
      <$> v
      .: "ok"
      <*> v
      .: "result"
  parseJSON _ = error "parser error"

instance ToJSON ResultClass where
  toJSON (ResultClass pollResultClass') =
    object
      [ "poll" .= pollResultClass'
      ]

instance FromJSON ResultClass where
  parseJSON (Object v) =
    ResultClass
      <$> v
      .: "poll"
  parseJSON _ = error "parser error"

instance ToJSON Poll where
  toJSON (Poll pollIDPoll') =
    object
      [ "id" .= pollIDPoll'
      ]

instance FromJSON Poll where
  parseJSON (Object v) =
    Poll
      <$> v
      .: "id"
  parseJSON _ = error "parser error"
