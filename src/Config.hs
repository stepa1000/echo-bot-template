-- | A module to provide a configuration reader for other modules.
module Config
  ( getBotConfig,
    getLoggerConfig,
    getFrontEndType,
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T

import qualified ConfigurationTypes
import qualified EchoBot
import qualified Logger.Impl

import Config

import Debug.Trace
-- import Data.Ini.Config

-- | Gets the bot config. In any case it can provide reasonable
-- default values.
getBotConfig :: IO EchoBot.Config
getBotConfig = do
   t <- T.readFile "config/EchoBot.yaml"
   case (fmap traceShowId (parse t) >>= parseValue) of
     (Right v) -> return $ undefined
     (Left errorParse) -> error errorParse
  where
    parseValue = return . id
{- do
  parseIniFile "config/EchoBot.ini" $ do
    section "ECHO_BOT" $ do
      textHelpReply <- field "confHelpReply"
      textRepeatReply <- field "confRepeatReply"
      intRepetitionCount <- fieldOf "confRepetitionCount" T.decimal
      return $ EchoBot.Config
        { confHelpReply = textHelpReply
        , confRepeatReply = textRepeatReply
        , confRepetitionCount = intRepetitionCount
        }
-}
getLoggerConfig :: IO Logger.Impl.Config
getLoggerConfig = error "Not implemented"

getFrontEndType :: IO ConfigurationTypes.FrontEndType
getFrontEndType = error "Not implemented"
