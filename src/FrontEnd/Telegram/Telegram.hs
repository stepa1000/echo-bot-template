{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | The telegram front-end is responsible for telegram api and
-- appropriate handling of other high-level bot interactions (menu
-- output etc).
module FrontEnd.Telegram.Telegram where

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Data.IORef
import Data.Map as M
import Data.Maybe
import Data.Semigroup
import Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Vector as V
import Data.Yaml
import qualified EchoBot
import qualified FrontEnd.Telegram.API as API
import qualified FrontEnd.Telegram.Data.GetUpdate as GU
import qualified FrontEnd.Telegram.Data.PollMessage as PM
import GHC.Generics
import Logger ((.<))
import qualified Logger
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Servant.Client
import qualified System.IO as SIO
import Prelude as P

data Handle m = Handle
  { hConfigTelegramBot :: Config,
    -- | Returns the last upadate count
    hGetLastUpdate :: m (Maybe Int),
    -- | Set the last update count
    hSetLastUpdate :: Maybe Int -> m (),
    -- | Returns the Accounting
    hGetAccounting :: m Accounting,
    -- | Modify the Accounting
    hModifyAccounting :: (Accounting -> Accounting) -> m (),
    -- | Lift for ClientM Monad
    hLiftClientM :: forall a. ClientM a -> m a,
    handleEchoBot :: EchoBot.Handle m AccountMessage
  }

logHandle :: Handle m -> Logger.Handle m
logHandle = EchoBot.hLogHandle . handleEchoBot

type UserId = Int

type ChatId = Int

type PollId = T.Text

data Accounting = Accounting
  { -- | Current identifier for chat and user
    currentAccountId :: AccountId,
    currentState :: EchoBot.State,
    -- | Current identifer for polls for single user
    currentPollID :: Maybe PollId,
    mapState :: Map UserId AccountState
  }

-- | State for one account
data AccountState = AccountState
  { -- | Identifier for chat and user
    accountId :: AccountId,
    accountState :: EchoBot.State,
    -- | Identifer for polls for single user
    accountPollId :: Maybe PollId
  }

data AccountId = AccountId
  { accountUserId :: UserId,
    accountIdChatId :: ChatId
  }
  deriving (Eq)

data AccountMessage
  = AccountMessage
      { accountMesageText :: T.Text
      }
  | AccountMessagePhoto
      { accountFileIDPhoto :: Set T.Text
      }

accountMessageToText :: AccountMessage -> Maybe T.Text
accountMessageToText (AccountMessage t) = Just t
accountMessageToText _ = Nothing

textToAccountMessage :: T.Text -> AccountMessage
textToAccountMessage = AccountMessage

data AccountEvent = AccountEvent
  { accountChatId :: ChatId,
    accountMessages :: Vector AccountMessage
  }

data AccountPoll = AccountPoll
  { accountUpdateIDPoll :: Int,
    accountIdPoll :: T.Text,
    accTotalVoterPoll :: Int,
    accOptionsPoll :: Vector GU.Option
  }
  deriving (Show)

newtype Config = Config
  { confBotToken :: T.Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | initiates a bot handle for ClientM
initHandleClientM ::
  Config ->
  EchoBot.Handle ClientM AccountMessage ->
  IORef Accounting ->
  IO (Handle ClientM)
initHandleClientM config ebHandle refAcc = do
  refLU <- newIORef Nothing
  return $
    Handle
      { hConfigTelegramBot = config,
        hGetLastUpdate = liftBase $ readIORef refLU,
        hSetLastUpdate = liftBase . writeIORef refLU,
        hGetAccounting = liftBase $ readIORef refAcc,
        hModifyAccounting = liftBase . modifyIORef refAcc,
        hLiftClientM = id,
        handleEchoBot = ebHandle
      }

run :: Monad m => Handle m -> m ()
run h = do
  -- Pulling up the counter of
  -- the last update to fresh messages
  _ <- hGetUpdates h
  run' h

run' :: Monad m => Handle m -> m ()
run' h = do
  runLoop h
  run' h

-- | main loop body
runLoop :: Monad m => Handle m -> m ()
runLoop h = do
  Logger.logInfo (logHandle h) "Start main telegram bot loop"
  vUp <- hGetUpdates h
  let (mapNAccount, mapAccPoll) = filterResult vUp
  P.sequence_ $ M.mapWithKey (updateAccount h) mapNAccount
  s <- hGetAccounting h
  P.sequence_ $ M.mapWithKey (\n -> updatePoll h n mapAccPoll) (mapState s)
  where
    filterResult = vResultTovAccountEvent

-- | Requests fresh messages and updates the counter of
-- | the last update
hGetUpdates :: Monad m => Handle m -> m (Vector GU.ResultElement)
hGetUpdates h = do
  mlastUp <- hGetLastUpdate h
  Logger.logDebug (logHandle h) $ "Current last update " .< mlastUp
  case mlastUp of
    (Just lUp) -> do
      (GU.WelcomeUpdate _ vUp) <- hLiftClientM h $ API.getUpdates (Just (lUp + 1))
      Logger.logDebug (logHandle h) $ "Last result vector length " .< V.length vUp
      Logger.logDebug (logHandle h) $ "Filtered result vector length " .< V.length (filterUpdate lUp vUp)
      Logger.logDebug (logHandle h) $ "Vector IDUpdates " .< fmap GU.updateIDResultElement vUp
      hSetLastUpdate h (Just $ max (getNewLastUpdate vUp) lUp)
      return $ filterUpdate lUp vUp
    _ -> do
      (GU.WelcomeUpdate _ vUp) <- hLiftClientM h $ API.getUpdates Nothing
      Logger.logDebug (logHandle h) $ "Last result vector length " .< V.length vUp
      hSetLastUpdate h (Just $ getNewLastUpdate vUp)
      return vUp

-- | Returns the maximum update id
getNewLastUpdate :: Vector GU.ResultElement -> Int
getNewLastUpdate = getMax . P.foldMap (Max . GU.updateIDResultElement)

-- | Returns a message with an id greater than the current one
filterUpdate :: Int -> Vector GU.ResultElement -> Vector GU.ResultElement
filterUpdate lastUp =
  V.filter
    ( \re ->
        lastUp < GU.updateIDResultElement re
    )

-- | Updates the account for each message
updateAccount :: Monad m => Handle m -> UserId -> AccountEvent -> m ()
updateAccount h n (AccountEvent chatID vMessage) = do
  Logger.logDebug (logHandle h) $ "Current UserId " .< n
  switchAccount h n chatID
  s1 <- hGetAccounting h
  Logger.logDebug (logHandle h) $ "Repetition count" .< currentState s1
  P.mapM_ (updateAccountMessage h chatID) (V.toList vMessage)
  refreshAccount h
  return ()

-- | Updates the account for poll
updatePoll :: Monad m => Handle m -> UserId -> Map PollId AccountPoll -> AccountState -> m ()
updatePoll h n mapNAP ast = do
  Logger.logDebug (logHandle h) "Update account poll "
  Logger.logDebug (logHandle h) $ "Poll account " .< accountPollId ast
  Logger.logDebug (logHandle h) $ "Map polls" .< mapNAP
  case accountPollId ast >>= (mapNAP M.!?) of
    (Just p) -> when (accTotalVoterPoll p > 0) $
      do
        let mO = P.foldl1 maxOption (accOptionsPoll p)
        Logger.logDebug (logHandle h) $ "Current poll maximum for option " .< mO
        switchAccount h n (accountIdChatId $ accountId ast)
        r <-
          EchoBot.respond
            (handleEchoBot h)
            ( EchoBot.SetRepetitionCountEvent $
                either (const (EchoBot.stRepetitionCount $ accountState ast)) fst $
                  T.decimal $
                    GU.optionTextOption mO
            )
        P.mapM_ (sendAccMessage h (accountIdChatId $ accountId ast)) r
        hModifyAccounting h (\s -> s {currentPollID = Nothing})
        s <- hGetAccounting h
        Logger.logDebug (logHandle h) $ "Repetition count modify" .< currentState s
        refreshAccount h
        Logger.logDebug (logHandle h) $ "Repetition count modify post \"refrashAccount\" " .< currentState s
        return ()
    _ -> return ()
  where
    maxOption o1 o2
      | GU.voterCountOption o1 >= GU.voterCountOption o2 = o1
      | otherwise = o2

-- | writes the current state of the account to the map of accounts
refreshAccount :: Monad m => Handle m -> m ()
refreshAccount h = do
  s <- hGetAccounting h
  switchAccount h (accountUserId $ currentAccountId s) (accountIdChatId $ currentAccountId s)

switchAccount :: Monad m => Handle m -> UserId -> Int -> m ()
switchAccount h n chatId = do
  s <- hGetAccounting h
  case mapState s M.!? n of
    (Just accSt) -> do
      hModifyAccounting h (\s2 -> s2 {mapState = f (mapState s) (currentAccountId s) (currentState s) (currentPollID s)})
      hModifyAccounting
        h
        ( \s2 ->
            s2
              { currentAccountId = AccountId n chatId,
                currentState = accountState accSt
              }
        )
    _ -> do
      hModifyAccounting h (\s2 -> s2 {mapState = f (mapState s) (currentAccountId s) (currentState s) (currentPollID s)})
      let conf = EchoBot.hConfig $ handleEchoBot h
      hModifyAccounting
        h
        ( \s2 ->
            s2
              { currentAccountId = AccountId n chatId,
                currentState = EchoBot.State $ EchoBot.confRepetitionCount conf
              }
        )
  where
    f mapSt currentAccId currentSt currentPollID' =
      M.insert
        (accountUserId currentAccId)
        (AccountState currentAccId currentSt currentPollID')
        mapSt

-- | Message processing for the current account
updateAccountMessage :: Monad m => Handle m -> Int -> AccountMessage -> m ()
updateAccountMessage h chatId x = do
  -- error "Not implement"
  lr <- EchoBot.respond (handleEchoBot h) (EchoBot.MessageEvent x)
  case lr of
    ((EchoBot.MenuResponse t lre) : _) -> do
      (PM.WelcomePoll _ r) <-
        hLiftClientM h $
          API.sendPoll
            (Just chatId)
            (Just t)
            (Just $ printToListText lre)
      Logger.logDebug (logHandle h) "Send Poll "
      hModifyAccounting h (\s -> s {currentPollID = Just $ PM.pollIDPoll $ PM.pollResultClass r})
    ys -> do
      Logger.logDebug (logHandle h) $ "Output repeat message " .< P.length ys
      P.mapM_ (sendAccMessage h chatId) ys
  where
    printToListText :: [(EchoBot.RepetitionCount, EchoBot.Event a)] -> API.ListText
    printToListText [] = API.ListText []
    printToListText ((y, _) : ys) = API.ListText $ (\lt -> T.pack (show y) : lt) $ API.unListText $ printToListText ys

-- | Send message or photo
sendAccMessage :: Monad m => Handle m -> Int -> EchoBot.Response AccountMessage -> m ()
sendAccMessage h chatId (EchoBot.MessageResponse (AccountMessage t)) = do
  _ <- hLiftClientM h $ API.sendMessage (Just chatId) (Just t)
  return ()
sendAccMessage h chatId (EchoBot.MessageResponse (AccountMessagePhoto sphid)) = do
  Logger.logDebug (logHandle h) $ "AccountMessagePhoto to list " .< P.length (S.elems sphid)
  _ <- hLiftClientM h $ API.sendPhoto (Just chatId) (Just $ S.findMax sphid)
  return ()
sendAccMessage _ _ _ = return ()

vResultTovAccountEvent :: Vector GU.ResultElement -> (Map UserId AccountEvent, Map PollId AccountPoll)
vResultTovAccountEvent = P.foldl f (M.empty, M.empty)
  where
    f (ma, mapoll) mre =
      fromJust $
        ( do
            re <- GU.messageResultElement mre
            accMessage <- readAccountMessage re
            return
              ( M.insertWith
                  g
                  (GU.fromIDFrom $ GU.fromMessage re)
                  ( AccountEvent
                      { accountChatId = GU.chatIDChat $ GU.chatMessage re,
                        accountMessages = V.singleton accMessage
                      }
                  )
                  ma,
                mapoll
              )
        )
          <|> ( do
                  re <- GU.pollResultElement mre
                  return
                    ( ma,
                      M.insertWith
                        accomulatePoll
                        (GU.pollIDPoll re)
                        ( AccountPoll
                            { accountUpdateIDPoll = GU.updateIDResultElement mre,
                              accountIdPoll = GU.pollIDPoll re,
                              accTotalVoterPoll = GU.totalVoterCountPoll re,
                              accOptionsPoll = GU.optionsPoll re
                            }
                        )
                        mapoll
                    )
              )
          <|> return (ma, mapoll)
    readAccountMessage (GU.Message _ _ _ (Just vphoto)) =
      Just $
        AccountMessagePhoto
          { accountFileIDPhoto = P.foldl (\s ph -> S.insert (GU.fileIDPhoto ph) s) S.empty vphoto
          }
    readAccountMessage (GU.Message _ _ (Just mText) _) =
      Just $
        AccountMessage
          { accountMesageText = mText
          }
    readAccountMessage _ = Nothing
    g (AccountEvent _ m1) (AccountEvent chatid m2) = AccountEvent chatid (m1 V.++ m2)
    accomulatePoll a1@(AccountPoll rid _ _ _) a2@(AccountPoll rid2 _ _ _)
      | rid > rid2 = a1
      | otherwise = a2

clientEnvDefault :: Config -> IO ClientEnv
clientEnvDefault conf = do
  m <- newManager tlsManagerSettings
  url <- parseBaseUrl $ "https://api.telegram.org/bot" P.++ T.unpack (confBotToken conf)
  return $ mkClientEnv m url

testUpdate' :: Config -> IO ()
testUpdate' conf = do
  m <- clientEnvDefault conf
  e <-
    runClientM
      ( do
          API.getUpdates (Just 1)
      )
      m
  SIO.print e
