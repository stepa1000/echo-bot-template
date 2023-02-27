{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A module to provide a configuration reader for other modules.
module ConfigBot
  ( GlobalConfig (..),
    getGlobalConfig,
    initGlobalConfig,
  )
where

import qualified ConfigurationTypes
import Data.Yaml
import qualified EchoBot
import qualified FrontEnd.Telegram.Telegram as Telegram
import GHC.Generics
import qualified Logger
import qualified Logger.Impl

data GlobalConfig = GlobalConfig
  { confEchoBot :: EchoBot.Config,
    confLogger :: Logger.Impl.PreConfig,
    confConfigurationTypes :: ConfigurationTypes.FrontEndType,
    confTelegram :: Telegram.Config
  }
  deriving (Generic, ToJSON, FromJSON)

getGlobalConfig :: IO (Either String GlobalConfig)
getGlobalConfig = do
  e <- decodeFileEither "config/global.yaml"
  case e of
    (Right gc) -> return $ Right gc
    (Left er) -> return $ Left $ prettyPrintParseException er

initGlobalConfig :: IO ()
initGlobalConfig = do
  encodeFile "config/global.yaml" $
    GlobalConfig
      { confEchoBot =
          EchoBot.Config
            { EchoBot.confHelpReply = "text for HelpReply",
              EchoBot.confRepeatReply = "text for RepeatReply",
              EchoBot.confRepetitionCount = 3
            },
        confLogger =
          Logger.Impl.PreConfig
            { Logger.Impl.preconfFilePath = "./logs/log.text",
              Logger.Impl.preconfMinLevel = Logger.Debug
            },
        confConfigurationTypes = ConfigurationTypes.TelegramFrontEnd,
        confTelegram =
          Telegram.Config
            { Telegram.confBotToken = ""
            }
      }
