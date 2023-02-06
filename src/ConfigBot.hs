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

getGlobalConfig :: IO (Either String GlobalConfig)
getGlobalConfig = do
  e <- decodeFileEither "config/global.yaml"
  case e of
    (Right gc) -> return $ Right gc
    (Left er) -> return $ Left $ errorToString er
  where
    errorToString NonScalarKey = "Non scalar key" -- show NonScalarKey
    errorToString (UnknownAlias an) = "Unknown alias: " ++ show an
    errorToString (UnexpectedEvent r e) = "Unexpectrd event: " ++ show r ++ " " ++ show e
    errorToString (InvalidYaml (Just (YamlException s))) = s
    errorToString (InvalidYaml (Just (YamlParseException p c pm ))) 
      = "Yaml parse exception: \n" ++ 
        "Problrm: " ++ show p ++ "\n" ++
        "Context: " ++ show c ++ "\n" ++
        "Problem mark: " ++ show pm 
    errorToString (InvalidYaml Nothing) = "Yaml parse exception: Nothing"
    errorToString MultipleDocuments = "Multiple documents"
    errorToString (AesonException s) = s
    errorToString (OtherParseException e) = show e
    errorToString (NonStringKey p) = "Non string key: " ++ show p
    errorToString (NonStringKeyAlias an v)
      = "Non string key alias: \n" ++
        "Anchor name: " ++ show an ++ "\n" ++
        "Value: " ++ show v
    errorToString CyclicIncludes = "Cyclic includes"
    errorToString (LoadSettingsException fp e)
      = "Load settings exception: \n" ++
        "File path: " ++ show fp ++ "\n" ++
        "Parse exception: " ++ show e
    

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


