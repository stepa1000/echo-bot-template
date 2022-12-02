{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module FrontEnd.Telegram.Data.SendMessage where

import Data.Aeson
import Data.Text
-- import Data.Vector

-- import FrontEnd.Telegram.Data.GetUpdate

data Welcome2 = Welcome2
    { okWelcome2 :: Bool
    , resultWelcome2 :: ResultClass
    } deriving (Show)

data ResultClass = ResultClass
    { messageIDResultClass :: Int
    , fromResultClass :: From
    , chatResultClass :: Chat
    , dateResultClass :: Int
    , resultTextResultClass :: Text
    } deriving (Show)

data Chat = Chat
    { chatIDChat :: Int
    , firstNameChat :: Text
    , chatTypeChat :: Text
    } deriving (Show)

data From = From
    { fromIDFrom :: Int
    , isBotFrom :: Bool
    , firstNameFrom :: Text
    , usernameFrom :: Text
    } deriving (Show)

-- decodeTopLevel :: ByteString -> Maybe Welcome2
-- decodeTopLevel = decode

instance ToJSON Welcome2 where
    toJSON (Welcome2 okWelcome2' resultWelcome2') =
        object
        [ "ok" .= okWelcome2'
        , "result" .= resultWelcome2'
        ]

instance FromJSON Welcome2 where
    parseJSON (Object v) = Welcome2
        <$> v .: "ok"
        <*> v .: "result"
    parseJSON _ = error "parser"

instance ToJSON ResultClass where
    toJSON (ResultClass messageIDResultClass' fromResultClass' chatResultClass' dateResultClass' resultTextResultClass') =
        object
        [ "message_id" .= messageIDResultClass'
        , "from" .= fromResultClass'
        , "chat" .= chatResultClass'
        , "date" .= dateResultClass'
        , "text" .= resultTextResultClass'
        ]

instance FromJSON ResultClass where
    parseJSON (Object v) = ResultClass
        <$> v .: "message_id"
        <*> v .: "from"
        <*> v .: "chat"
        <*> v .: "date"
        <*> v .: "text"
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

instance ToJSON From where
    toJSON (From fromIDFrom' isBotFrom' firstNameFrom' usernameFrom') =
        object
        [ "id" .= fromIDFrom'
        , "is_bot" .= isBotFrom'
        , "first_name" .= firstNameFrom'
        , "username" .= usernameFrom'
        ]

instance FromJSON From where
    parseJSON (Object v) = From
        <$> v .: "id"
        <*> v .: "is_bot"
        <*> v .: "first_name"
        <*> v .: "username"
    parseJSON _ = error "parser"

