{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module FrontEnd.Telegram.Data.SendPhoto where

import Data.Aeson
import Data.Text
import Data.ByteString.Lazy
import Data.Vector

data Welcome = Welcome
    { okWelcome :: Bool
    , resultWelcome10 :: ResultClass
    } deriving (Show)

data ResultClass = ResultClass
    { messageIDResultClass :: Int
    , fromResultClass :: From
    , chatResultClass :: Chat
    , dateResultClass :: Int
    , photoResultClass :: Vector Photo
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

data Photo = Photo
    { fileIDPhoto :: Text
    , fileUniqueIDPhoto :: Text
    , fileSizePhoto :: Int
    , widthPhoto :: Int
    , heightPhoto :: Int
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe Welcome
decodeTopLevel = decode

instance ToJSON Welcome where
    toJSON (Welcome okWelcome' resultWelcome') =
        object
        [ "ok" .= okWelcome'
        , "result" .= resultWelcome'
        ]

instance FromJSON Welcome where
    parseJSON (Object v) = Welcome
        <$> v .: "ok"
        <*> v .: "result"
    parseJSON _ = error "parser error"

instance ToJSON ResultClass where
    toJSON (ResultClass messageIDResultClass' fromResultClass' chatResultClass' dateResultClass' photoResultClass') =
        object
        [ "message_id" .= messageIDResultClass'
        , "from" .= fromResultClass'
        , "chat" .= chatResultClass'
        , "date" .= dateResultClass'
        , "photo" .= photoResultClass'
        ]

instance FromJSON ResultClass where
    parseJSON (Object v) = ResultClass
        <$> v .: "message_id"
        <*> v .: "from"
        <*> v .: "chat"
        <*> v .: "date"
        <*> v .: "photo"
    parseJSON _ = error "parser error"

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
    parseJSON _ = error "parser error"

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
    parseJSON _ = error "parser error"

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
    parseJSON _ = error "parser error"

