{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A module to provide a configuration reader for other modules.
module ConfigBot
  ( GlobalConfig (..)
  , getGlobalConfig
  , initGlobalConfig
  )
where

import GHC.Generics

-- import qualified Data.Text as T
-- import qualified Data.Text.IO as T
-- import qualified Data.Text.Read as T
-- import qualified System.IO as SIO

import qualified ConfigurationTypes
import qualified EchoBot
import qualified Logger.Impl
import qualified Logger

import qualified FrontEnd.Telegram.Telegram as Telegram

import Data.Yaml

data GlobalConfig = GlobalConfig
  { confEchoBot :: EchoBot.Config
  , confLogger :: Logger.Impl.PreConfig
  , confConfigurationTypes :: ConfigurationTypes.FrontEndType
  , confTelegram :: Telegram.Config
  } deriving (Generic, ToJSON, FromJSON)

getGlobalConfig :: IO GlobalConfig
getGlobalConfig = decodeFileThrow "config/global.yaml"

initGlobalConfig :: IO ()
initGlobalConfig = do
  encodeFile "config/global.yaml" $ GlobalConfig
    { confEchoBot = EchoBot.Config
      { EchoBot.confHelpReply = "text for HelpReply"
      , EchoBot.confRepeatReply = "text for RepeatReply"
      , EchoBot.confRepetitionCount = 3
      }
    , confLogger = Logger.Impl.PreConfig
      { Logger.Impl.preconfFilePath = "./logs/log.text"
      , Logger.Impl.preconfMinLevel = Logger.Debug
      }
    , confConfigurationTypes = ConfigurationTypes.TelegramFrontEnd
    , confTelegram = Telegram.Config
      { Telegram.confBotToken = ""
      }
    }


