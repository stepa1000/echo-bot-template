{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module FrontEnd.Telegram.Data.GetUpdate where

import Data.Aeson
import Data.Text
import Data.Vector

data Welcome10 = Welcome10
    { okWelcome10 :: Bool
    , resultWelcome10 :: Vector ResultElement
    } deriving (Show)

data ResultElement = ResultElement
    { updateIDResultElement :: Int
    , messageResultElement :: Message
    } deriving (Show)

data Message = Message
    { messageIDMessage :: Int
    , fromMessage :: From
    , chatMessage :: Chat
    , dateMessage :: Int
    , messageTextMessage :: Text
    , entitiesMessage :: Maybe (Vector Entity)
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

-- decodeTopLevel :: ByteString -> Maybe Welcome10
-- decodeTopLevel = decode

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
    toJSON (ResultElement updateIDResultElement' messageResultElement') =
        object
        [ "update_id" .= updateIDResultElement'
        , "message" .= messageResultElement'
        ]

instance FromJSON ResultElement where
    parseJSON (Object v) = ResultElement
        <$> v .: "update_id"
        <*> v .: "message"
    parseJSON _ = error "parser"

instance ToJSON Message where
    toJSON (Message messageIDMessage' fromMessage' chatMessage' dateMessage' messageTextMessage' entitiesMessage') =
        object
        [ "message_id" .= messageIDMessage'
        , "from" .= fromMessage'
        , "chat" .= chatMessage'
        , "date" .= dateMessage'
        , "text" .= messageTextMessage'
        , "entities" .= entitiesMessage'
        ]

instance FromJSON Message where
    parseJSON (Object v) = Message
        <$> v .: "message_id"
        <*> v .: "from"
        <*> v .: "chat"
        <*> v .: "date"
        <*> v .: "text"
        <*> v .:? "entities"
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

