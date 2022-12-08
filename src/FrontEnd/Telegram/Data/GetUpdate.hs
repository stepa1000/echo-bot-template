{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module FrontEnd.Telegram.Data.GetUpdate where

import Data.Aeson
import Data.Text
import Data.Vector
import Data.ByteString.Lazy

data Welcome10 = Welcome10
    { okWelcome10 :: Bool
    , resultWelcome10 :: Vector ResultElement
    } deriving (Show)

data ResultElement = ResultElement
    { updateIDResultElement :: Int
    , messageResultElement :: Maybe Message
    , pollResultElement :: Maybe Poll
    } deriving (Show)

data Message = Message
    { messageIDMessage :: Int
    , fromMessage :: From
    , chatMessage :: Chat
    , dateMessage :: Int
    , messageTextMessage :: Maybe Text
    , entitiesMessage :: Maybe (Vector Entity)
    , photoMessage :: Maybe (Vector Photo)
    } deriving (Show)

data Chat = Chat
    { chatIDChat :: Int
    , firstNameChat :: Text
    , chatTypeChat :: Text
    } deriving (Show)

data Entity = Entity
    { offsetEntity :: Int
    , lengthEntity :: Int
    , entityTypeEntity :: Text
    } deriving (Show)

data From = From
    { fromIDFrom :: Int
    , isBotFrom :: Bool
    , firstNameFrom :: Text
    , languageCodeFrom :: Text
    } deriving (Show)

data Photo = Photo
    { fileIDPhoto :: Text
    , fileUniqueIDPhoto :: Text
    , fileSizePhoto :: Int
    , widthPhoto :: Int
    , heightPhoto :: Int
    } deriving (Show)

data Poll = Poll
    { pollIDPoll :: Text
    , questionPoll :: Text
    , optionsPoll :: Vector Option
    , totalVoterCountPoll :: Int
    , isClosedPoll :: Bool
    , isAnonymousPoll :: Bool
    , pollTypePoll :: Text
    , allowsMultipleAnswersPoll :: Bool
    } deriving (Show)

data Option = Option
    { optionTextOption :: Text
    , voterCountOption :: Int
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe Welcome10
decodeTopLevel = decode

instance ToJSON Welcome10 where
    toJSON (Welcome10 okWelcome10' resultWelcome10') =
        object
        [ "ok" .= okWelcome10'
        , "result" .= resultWelcome10'
        ]

instance FromJSON Welcome10 where
    parseJSON (Object v) = Welcome10
        <$> v .: "ok"
        <*> v .: "result"
    parseJSON _ = error "parser"

instance ToJSON ResultElement where
    toJSON (ResultElement updateIDResultElement' messageResultElement' pollResultElement') =
        object
        [ "update_id" .= updateIDResultElement'
        , "message" .= messageResultElement'
        , "poll" .= pollResultElement'
        ]

instance FromJSON ResultElement where
    parseJSON (Object v) = ResultElement
        <$> v .: "update_id"
        <*> v .:? "message"
        <*> v .:? "poll"
    parseJSON _ = error "parser"

instance ToJSON Message where
    toJSON (Message messageIDMessage' fromMessage' chatMessage' dateMessage' messageTextMessage' entitiesMessage' photoMessage') =
        object
        [ "message_id" .= messageIDMessage'
        , "from" .= fromMessage'
        , "chat" .= chatMessage'
        , "date" .= dateMessage'
        , "text" .= messageTextMessage'
        , "entities" .= entitiesMessage'
        , "photo" .= photoMessage'
        ]

instance FromJSON Message where
    parseJSON (Object v) = Message
        <$> v .: "message_id"
        <*> v .: "from"
        <*> v .: "chat"
        <*> v .: "date"
        <*> v .:? "text"
        <*> v .:? "entities"
        <*> v .:? "photo"
    parseJSON _ = error "parser"

instance ToJSON Chat where
    toJSON (Chat chatIDChat' firstNameChat' chatTypeChat') =
        object
        [ "id" .= chatIDChat'
        , "first_name" .= firstNameChat'
        , "type" .= chatTypeChat'
        ]

instance FromJSON Chat where
    parseJSON (Object v) = Chat
        <$> v .: "id"
        <*> v .: "first_name"
        <*> v .: "type"
    parseJSON _ = error "parser"

instance ToJSON Entity where
    toJSON (Entity offsetEntity' lengthEntity' entityTypeEntity') =
        object
        [ "offset" .= offsetEntity'
        , "length" .= lengthEntity'
        , "type" .= entityTypeEntity'
        ]

instance FromJSON Entity where
    parseJSON (Object v) = Entity
        <$> v .: "offset"
        <*> v .: "length"
        <*> v .: "type"
    parseJSON _ = error "parser"

instance ToJSON From where
    toJSON (From fromIDFrom' isBotFrom' firstNameFrom' languageCodeFrom') =
        object
        [ "id" .= fromIDFrom'
        , "is_bot" .= isBotFrom'
        , "first_name" .= firstNameFrom'
        , "language_code" .= languageCodeFrom'
        ]

instance FromJSON From where
    parseJSON (Object v) = From
        <$> v .: "id"
        <*> v .: "is_bot"
        <*> v .: "first_name"
        <*> v .: "language_code"
    parseJSON _ = error "parser"

instance ToJSON Photo where
    toJSON (Photo fileIDPhoto' fileUniqueIDPhoto' fileSizePhoto' widthPhoto' heightPhoto') =
        object
        [ "file_id" .= fileIDPhoto'
        , "file_unique_id" .= fileUniqueIDPhoto'
        , "file_size" .= fileSizePhoto'
        , "width" .= widthPhoto'
        , "height" .= heightPhoto'
        ]

instance FromJSON Photo where
    parseJSON (Object v) = Photo
        <$> v .: "file_id"
        <*> v .: "file_unique_id"
        <*> v .: "file_size"
        <*> v .: "width"
        <*> v .: "height"
    parseJSON _ = error "parser"
    

instance ToJSON Poll where
    toJSON (Poll pollIDPoll' questionPoll' optionsPoll' totalVoterCountPoll' isClosedPoll' isAnonymousPoll' pollTypePoll' allowsMultipleAnswersPoll') =
        object
        [ "id" .= pollIDPoll'
        , "question" .= questionPoll'
        , "options" .= optionsPoll'
        , "total_voter_count" .= totalVoterCountPoll'
        , "is_closed" .= isClosedPoll'
        , "is_anonymous" .= isAnonymousPoll'
        , "type" .= pollTypePoll'
        , "allows_multiple_answers" .= allowsMultipleAnswersPoll'
        ]

instance FromJSON Poll where
    parseJSON (Object v) = Poll
        <$> v .: "id"
        <*> v .: "question"
        <*> v .: "options"
        <*> v .: "total_voter_count"
        <*> v .: "is_closed"
        <*> v .: "is_anonymous"
        <*> v .: "type"
        <*> v .: "allows_multiple_answers"
    parseJSON _ = error "parser"


instance ToJSON Option where
    toJSON (Option optionTextOption' voterCountOption') =
        object
        [ "text" .= optionTextOption'
        , "voter_count" .= voterCountOption'
        ]

instance FromJSON Option where
    parseJSON (Object v) = Option
        <$> v .: "text"
        <*> v .: "voter_count"
    parseJSON _ = error "parser"
