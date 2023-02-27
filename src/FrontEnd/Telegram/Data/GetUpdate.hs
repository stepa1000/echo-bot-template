{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module FrontEnd.Telegram.Data.GetUpdate where

import Data.Aeson
import Data.ByteString.Lazy
import Data.Text
import Data.Vector

data ResponseUpdate
  = ResponseUpdate
      { okResponeUpdate :: Bool,
        resultResponseUpdate :: Vector ResultElement
      }
  deriving (Show)

data ResultElement
  = ResultElement
      { updateIDResultElement :: Int,
        messageResultElement :: Maybe Message,
        pollResultElement :: Maybe Poll
      }
  deriving (Show)

data Message
  = Message
      { fromMessage :: FromMessage,
        chatMessage :: ChatMessage,
        messageTextMessage :: Maybe Text,
        photoMessage :: Maybe (Vector Photo)
      }
  deriving (Show)

data ChatMessage
  = Chat
      { chatIDChat :: Int
      }
  deriving (Show)

data FromMessage
  = From
      { fromIDFrom :: Int
      }
  deriving (Show)

data Photo
  = Photo
      { fileIDPhoto :: Text
      }
  deriving (Show)

data Poll
  = Poll
      { pollIDPoll :: Text,
        optionsPoll :: Vector OptionPoll,
        totalVoterCountPoll :: Int
      }
  deriving (Show)

data OptionPoll
  = Option
      { optionTextOption :: Text,
        voterCountOption :: Int
      }
  deriving (Show)

decodeTopLevel :: ByteString -> Maybe ResponseUpdate
decodeTopLevel = decode

instance ToJSON ResponseUpdate where
  toJSON (ResponseUpdate okResponse' resultResponse') =
    object
      [ "ok" .= okResponse',
        "result" .= resultResponse'
      ]

instance FromJSON ResponseUpdate where
  parseJSON (Object v) =
    ResponseUpdate
      <$> v .: "ok"
      <*> v .: "result"
  parseJSON _ = error "parser"

instance ToJSON ResultElement where
  toJSON (ResultElement updateIDResultElement' messageResultElement' pollResultElement') =
    object
      [ "update_id" .= updateIDResultElement',
        "message" .= messageResultElement',
        "poll" .= pollResultElement'
      ]

instance FromJSON ResultElement where
  parseJSON (Object v) =
    ResultElement
      <$> v .: "update_id"
      <*> v .:? "message"
      <*> v .:? "poll"
  parseJSON _ = error "parser"

instance ToJSON Message where
  toJSON (Message fromMessage' chatMessage' messageTextMessage' photoMessage') =
    object
      [ "from" .= fromMessage',
        "chat" .= chatMessage',
        "text" .= messageTextMessage',
        "photo" .= photoMessage'
      ]

instance FromJSON Message where
  parseJSON (Object v) =
    Message
      <$> v .: "from"
      <*> v .: "chat"
      <*> v .:? "text"
      <*> v .:? "photo"
  parseJSON _ = error "parser"

instance ToJSON ChatMessage where
  toJSON (Chat chatIDChat') =
    object
      [ "id" .= chatIDChat'
      ]

instance FromJSON ChatMessage where
  parseJSON (Object v) =
    Chat
      <$> v .: "id"
  parseJSON _ = error "parser"

instance ToJSON FromMessage where
  toJSON (From fromIDFrom') =
    object
      [ "id" .= fromIDFrom'
      ]

instance FromJSON FromMessage where
  parseJSON (Object v) =
    From
      <$> v .: "id"
  parseJSON _ = error "parser"

instance ToJSON Photo where
  toJSON (Photo fileIDPhoto') =
    object
      [ "file_id" .= fileIDPhoto'
      ]

instance FromJSON Photo where
  parseJSON (Object v) =
    Photo
      <$> v .: "file_id"
  parseJSON _ = error "parser"

instance ToJSON Poll where
  toJSON (Poll pollIDPoll' optionsPoll' totalVoterCountPoll') =
    object
      [ "id" .= pollIDPoll',
        "options" .= optionsPoll',
        "total_voter_count" .= totalVoterCountPoll'
      ]

instance FromJSON Poll where
  parseJSON (Object v) =
    Poll
      <$> v .: "id"
      <*> v .: "options"
      <*> v .: "total_voter_count"
  parseJSON _ = error "parser"

instance ToJSON OptionPoll where
  toJSON (Option optionTextOption' voterCountOption') =
    object
      [ "text" .= optionTextOption',
        "voter_count" .= voterCountOption'
      ]

instance FromJSON OptionPoll where
  parseJSON (Object v) =
    Option
      <$> v .: "text"
      <*> v .: "voter_count"
  parseJSON _ = error "parser"
