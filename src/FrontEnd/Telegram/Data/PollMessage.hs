{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module FrontEnd.Telegram.Data.PollMessage where

import Data.Aeson
import Data.ByteString.Lazy
import Data.Text

data WelcomePoll
  = WelcomePoll
      { okWelcomePoll :: Bool,
        resultWelcomePoll :: ResultClass
      }
  deriving (Show)

data ResultClass
  = ResultClass
      { pollResultClass :: Poll
      }
  deriving (Show)

data Poll
  = Poll
      { pollIDPoll :: Text
      }
  deriving (Show)

decodeTopLevel :: ByteString -> Maybe WelcomePoll
decodeTopLevel = decode

instance ToJSON WelcomePoll where
  toJSON (WelcomePoll okWelcome9' resultWelcome9') =
    object
      [ "ok" .= okWelcome9',
        "result" .= resultWelcome9'
      ]

instance FromJSON WelcomePoll where
  parseJSON (Object v) =
    WelcomePoll
      <$> v .: "ok"
      <*> v .: "result"
  parseJSON _ = error "parser error"

instance ToJSON ResultClass where
  toJSON (ResultClass pollResultClass') =
    object
      [ "poll" .= pollResultClass'
      ]

instance FromJSON ResultClass where
  parseJSON (Object v) =
    ResultClass
      <$> v .: "poll"
  parseJSON _ = error "parser error"

instance ToJSON Poll where
  toJSON (Poll pollIDPoll') =
    object
      [ "id" .= pollIDPoll'
      ]

instance FromJSON Poll where
  parseJSON (Object v) =
    Poll
      <$> v .: "id"
  parseJSON _ = error "parser error"
