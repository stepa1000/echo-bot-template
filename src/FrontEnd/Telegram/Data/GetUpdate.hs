{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module FrontEnd.Telegram.Data.GetUpdate where

import Data.Aeson
import Data.ByteString.Lazy
import Data.Text
import Data.Vector

data WelcomeUpdate
  = WelcomeUpdate
      { okWelcomeUpdate :: Bool,
        resultWelcomeUpdate :: Vector ResultElement
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
      { fromMessage :: From,
        chatMessage :: Chat,
        messageTextMessage :: Maybe Text,
        photoMessage :: Maybe (Vector Photo)
      }
  deriving (Show)

data Chat
  = Chat
      { chatIDChat :: Int
      }
  deriving (Show)

data From
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
        optionsPoll :: Vector Option,
        totalVoterCountPoll :: Int
      }
  deriving (Show)

data Option
  = Option
      { optionTextOption :: Text,
        voterCountOption :: Int
      }
  deriving (Show)

decodeTopLevel :: ByteString -> Maybe WelcomeUpdate
decodeTopLevel = decode

instance ToJSON WelcomeUpdate where
  toJSON (WelcomeUpdate okWelcome10' resultWelcome10') =
    object
      [ "ok" .= okWelcome10',
        "result" .= resultWelcome10'
      ]

instance FromJSON WelcomeUpdate where
  parseJSON (Object v) =
    WelcomeUpdate
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

instance ToJSON Chat where
  toJSON (Chat chatIDChat') =
    object
      [ "id" .= chatIDChat'
      ]

instance FromJSON Chat where
  parseJSON (Object v) =
    Chat
      <$> v .: "id"
  parseJSON _ = error "parser"

instance ToJSON From where
  toJSON (From fromIDFrom') =
    object
      [ "id" .= fromIDFrom'
      ]

instance FromJSON From where
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

instance ToJSON Option where
  toJSON (Option optionTextOption' voterCountOption') =
    object
      [ "text" .= optionTextOption',
        "voter_count" .= voterCountOption'
      ]

instance FromJSON Option where
  parseJSON (Object v) =
    Option
      <$> v .: "text"
      <*> v .: "voter_count"
  parseJSON _ = error "parser"
