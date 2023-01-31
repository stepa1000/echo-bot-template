{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module FrontEnd.Telegram.Data.PollMessage where

import Data.Aeson
import Data.Text
import Data.Vector
import Data.ByteString.Lazy

{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

data WelcomePoll = WelcomePoll -- 9
    { okWelcomePoll :: Bool
    , resultWelcomePoll :: ResultClass
    } deriving (Show)

data ResultClass = ResultClass
    { messageIDResultClass :: Int
    , fromResultClass :: From
    , chatResultClass :: Chat
    , dateResultClass :: Int
    , pollResultClass :: Poll
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

decodeTopLevel :: ByteString -> Maybe WelcomePoll
decodeTopLevel = decode

instance ToJSON WelcomePoll where
    toJSON (WelcomePoll okWelcome9' resultWelcome9') =
        object
        [ "ok" .= okWelcome9'
        , "result" .= resultWelcome9'
        ]

instance FromJSON WelcomePoll where
    parseJSON (Object v) = WelcomePoll
        <$> v .: "ok"
        <*> v .: "result"
    parseJSON _ = error "parser error"

instance ToJSON ResultClass where
    toJSON (ResultClass messageIDResultClass' fromResultClass' chatResultClass' dateResultClass' pollResultClass') =
        object
        [ "message_id" .= messageIDResultClass'
        , "from" .= fromResultClass'
        , "chat" .= chatResultClass'
        , "date" .= dateResultClass'
        , "poll" .= pollResultClass'
        ]

instance FromJSON ResultClass where
    parseJSON (Object v) = ResultClass
        <$> v .: "message_id"
        <*> v .: "from"
        <*> v .: "chat"
        <*> v .: "date"
        <*> v .: "poll"
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
    parseJSON _ = error "parser error"

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
    parseJSON _ = error "parser error"

