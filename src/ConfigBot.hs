-- | A module to provide a configuration reader for other modules.
module ConfigBot
  ( getBotConfig,
    getLoggerConfig,
    getFrontEndType,
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as T
-- import qualified Data.Text.Read as T
import qualified System.IO as SIO

import qualified ConfigurationTypes
import qualified EchoBot
import qualified Logger.Impl
import qualified Logger

import Config as C

-- import Debug.Trace
-- import Data.Ini.Config

-- | Gets the bot config. In any case it can provide reasonable
-- default values.
getBotConfig :: IO EchoBot.Config
getBotConfig = do
   t <- T.readFile "config/EchoBot.yaml"
   case parse t >>= parseValue of
     (Right v) -> return $ v
     (Left errorParse) -> error $ show errorParse
  where
    parseValue 
      (Sections _ ( (Section _ _ (Sections _ (
        (Section _ _ (C.Text _ t1)) :
        (Section _ _ (C.Text _ t2) ) :
        (Section _ _ (Number _ n)) :
        [] )
        )
      ) : [])) = return $ EchoBot.Config
        { EchoBot.confHelpReply = t1
        , EchoBot.confRepeatReply = t2
        , EchoBot.confRepetitionCount = ceiling $ numberToRational $ n
        }
    parseValue _ = error "parseValue patern matching error"


getLoggerConfig :: IO Logger.Impl.Config
getLoggerConfig = do -- error "Not implemented"
  t <- T.readFile "config/LoggerImp.yaml"
  case parse t of
     (Right v) -> parseValue v
     (Left errorParse) -> error $ show errorParse
  where
    parseValue (Sections _ ( (Section _ _ (Sections _ (
        (Section _ _ (C.Text _ t1)) :
        (Section _ _ (C.Text _ t2)) :
        [] )
        )
      ) : [])) = do
        h <- SIO.openFile (T.unpack t1) SIO.WriteMode
        return $ Logger.Impl.Config 
          { Logger.Impl.confFileHandle = h
          , Logger.Impl.confMinLevel = Logger.textToLogLvl t2
          } 
    parseValue _ = error "parseValue patern matching error"

 
getFrontEndType :: IO ConfigurationTypes.FrontEndType
getFrontEndType = do -- error "Not implemented"
  t <- T.readFile "config/ConfigurationTypes.yaml"
  case parse t of
     (Right v) -> parseValue v
     (Left errorParse) -> error $ show errorParse
  where
    parseValue (Sections _ ( (Section _ _ (C.Text _ t1) ) : [])) = do
        return $ ConfigurationTypes.textToFrontEndType $ t1
    parseValue _ = error "parseValue patern matching error"
