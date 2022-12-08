{-# LANGUAGE TypeApplications #-}

module Main
  ( main,
  )
where

import Prelude as P

import Servant.Client

import qualified Data.Map as M

import qualified Control.Monad.State.Lazy as SL

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
  withLogHandle $ \logHandle -> do
    frontEnd <- ConfigBot.getFrontEndType
    case frontEnd of
      ConfigurationTypes.TelegramFrontEnd -> do
        botHandle <- makeBotHandleForTelegram $ 
          Logger.Impl.liftHandleBaseIO logHandle 
        telegramConfig <- ConfigBot.getTelegramConfig
        execStateAccounting $ Telegram.withHandle telegramConfig botHandle $ \ telegramHandle ->
          Telegram.run telegramHandle
      ConfigurationTypes.ConsoleFrontEnd -> do
        botHandle <- makeBotHandleForPlainText logHandle
        runConsoleFrontEnd botHandle

runConsoleFrontEnd :: EchoBot.Handle IO T.Text -> IO ()
runConsoleFrontEnd botHandle =
  FrontEnd.Console.run
    FrontEnd.Console.Handle {FrontEnd.Console.hBotHandle = botHandle}

withLogHandle :: (Logger.Handle IO -> IO ()) -> IO ()
withLogHandle f = do
  config <- ConfigBot.getLoggerConfig
  Logger.Impl.withHandle config f

execStateAccounting :: SL.StateT Telegram.Accounting ClientM a -> IO a
execStateAccounting (SL.StateT st) = do
  botConfig <- ConfigBot.getBotConfig
  initialState <- either (die . T.unpack) pure $ EchoBot.makeState botConfig
  telegramConfig <- ConfigBot.getTelegramConfig
  e <- Telegram.clientEnvDefault telegramConfig
  a <- runClientM (st $ Telegram.Accounting
    { Telegram.currentAccountId = Telegram.AccountId 
      { Telegram.accountUserId  = 5950752982
      , Telegram.accountIdChatId = 889933266
      }
    , Telegram.currentState = initialState
    , Telegram.currentPollID = Nothing
    , Telegram.mapState = M.empty
    }) e
  case a of
    (Right (b,_) ) -> return b
    (Left (DecodeFailure t b )) -> do
      let js = decode @Aeson.Value $ responseBody b
      encodeFile "./logs/responseBody.json" js
      error $ (show t) P.++ "\n" P.++
              (ppShow b) P.++ "\n" P.++ 
              (ppShow $ js)
    (Left b) -> error (ppShow b)

makeBotHandleForTelegram :: Logger.Handle (SL.StateT Telegram.Accounting ClientM) 
                         -> IO (EchoBot.Handle 
                           (SL.StateT Telegram.Accounting ClientM) 
                           Telegram.AccountMessage
                           )
makeBotHandleForTelegram logHandle = do
  botConfig <- ConfigBot.getBotConfig
  -- initialState <- either (die . T.unpack) pure $ EchoBot.makeState botConfig
  return $ EchoBot.Handle
    { EchoBot.hGetState = fmap (Telegram.currentState) $ SL.get
    , EchoBot.hModifyState' = \ f -> SL.modify 
      (\s->s{Telegram.currentState = f $ Telegram.currentState s })
    , EchoBot.hLogHandle = logHandle
    , EchoBot.hConfig = botConfig
    , EchoBot.hTextFromMessage = Telegram.accountMessageToText
    , EchoBot.hMessageFromText = Telegram.textToAccountMessage
    }

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
makeBotHandleForPlainText :: Logger.Handle IO -> IO (EchoBot.Handle IO T.Text)
makeBotHandleForPlainText logHandle = do
  botConfig <- ConfigBot.getBotConfig
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
