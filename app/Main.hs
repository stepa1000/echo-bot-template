{-# LANGUAGE TypeApplications #-}

module Main
  ( main,
  )
where

import Prelude as P

import Servant.Client

import qualified Data.Map as M

import Control.Monad.Base

import qualified ConfigBot
import qualified ConfigurationTypes
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Text as T
import qualified EchoBot
import qualified FrontEnd.Console
import qualified FrontEnd.Telegram.Telegram as Telegram
import qualified Logger
import qualified Logger.Impl
import System.Exit (die)

import Data.Aeson as Aeson
import Text.Show.Pretty

main :: IO ()
main = do
  gconfE <- ConfigBot.getGlobalConfig
  case gconfE of
    (Right gconf) -> Logger.Impl.withPreConf (ConfigBot.confLogger gconf) $ \logHandle -> do
      case ConfigBot.confConfigurationTypes gconf of
        ConfigurationTypes.TelegramFrontEnd -> do
          telegramHandle <- makeBotHandleForTelegram (ConfigBot.confTelegram gconf) (ConfigBot.confEchoBot gconf) $
            Logger.Impl.liftHandleBaseIO logHandle 
          execClientM (ConfigBot.confTelegram gconf) $ Telegram.run telegramHandle
        ConfigurationTypes.ConsoleFrontEnd -> do
          botHandle <- makeBotHandleForPlainText (ConfigBot.confEchoBot gconf) logHandle
          runConsoleFrontEnd botHandle
    (Left er) -> do
      pPrint er

runConsoleFrontEnd :: EchoBot.Handle IO T.Text -> IO ()
runConsoleFrontEnd botHandle =
  FrontEnd.Console.run
    FrontEnd.Console.Handle {FrontEnd.Console.hBotHandle = botHandle}

execClientM :: Telegram.Config -> ClientM a -> IO a
execClientM telegramConfig m = do
  e <- Telegram.clientEnvDefault telegramConfig
  a <- runClientM m e
  case a of
    (Right b) -> return b
    (Left (DecodeFailure t b )) -> do
      let js = decode @Aeson.Value $ responseBody b
      encodeFile "./logs/responseBody.json" js
      error $ (show t) P.++ "\n" P.++
              (ppShow b) P.++ "\n" P.++ 
              (ppShow $ js)
    (Left (FailureResponse req res)) -> do
      let js = decode @Aeson.Value $ responseBody res
      encodeFile "./logs/responseBody.json" js
      error $ (show req) P.++ "\n" P.++
              (ppShow res) P.++ "\n" P.++ 
              (ppShow $ js)
    (Left b) -> error (ppShow b)

makeBotHandleForTelegram :: Telegram.Config
                         -> EchoBot.Config
                         -> Logger.Handle ClientM 
                         -> IO (Telegram.Handle ClientM)
makeBotHandleForTelegram telegramConfig botConfig logHandle = do
  initialState <- either (die . T.unpack) pure $ EchoBot.makeState botConfig
  refAcc <- newIORef $ Telegram.Accounting
    { Telegram.currentAccountId = Telegram.AccountId 
      { Telegram.accountUserId  = 0 
      , Telegram.accountIdChatId = 0 
      }
    , Telegram.currentState = initialState
    , Telegram.currentPollID = Nothing
    , Telegram.mapState = M.empty
    }  
  Telegram.initHandleClientM 
    telegramConfig 
    (EchoBot.Handle
      { EchoBot.hGetState = liftBase $ fmap Telegram.currentState $ readIORef refAcc
      , EchoBot.hModifyState' = \ f -> liftBase $ 
          modifyIORef' refAcc (\s->s {Telegram.currentState = f $ Telegram.currentState s})
      , EchoBot.hLogHandle = logHandle
      , EchoBot.hConfig = botConfig
      , EchoBot.hTextFromMessage = Telegram.accountMessageToText
      , EchoBot.hMessageFromText = Telegram.textToAccountMessage
      }
    )
    refAcc

-- | Creates a bot handle. Please note:
--
-- * a handle holds a reference to an 'IORef' with a 'EchoBot.State',
--   so that it can only keep state of a single user. In order to
--   support multiple users in a chat, you should create a new handle
--   for each user and probably keep them in a 'Data.Map' keyed by a
--   user id.
--
-- * 'EchoBot.Handle' is parameterized with the 'Text' type, so that
--   it supports only plain text messages suitable for the console.
--   When implementing Telegram or another multimedia chat support,
--   you should create a similar function, but parameterized with
--   another message type which can represent either text or
--   multimedia messages. You will need to specify different functions
--   @hMessageFromText@ and @hTextFromMessage@.
makeBotHandleForPlainText :: EchoBot.Config -> Logger.Handle IO -> IO (EchoBot.Handle IO T.Text)
makeBotHandleForPlainText botConfig logHandle = do
  initialState <- either (die . T.unpack) pure $ EchoBot.makeState botConfig
  stateRef <- newIORef initialState
  pure
    EchoBot.Handle
      { EchoBot.hGetState = readIORef stateRef,
        EchoBot.hModifyState' = modifyIORef' stateRef,
        EchoBot.hLogHandle = logHandle,
        EchoBot.hConfig = botConfig,
        EchoBot.hTextFromMessage = Just,
        EchoBot.hMessageFromText = id
      }
