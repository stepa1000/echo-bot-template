{-# LANGUAGE OverloadedStrings #-}

-- | The telegram front-end is responsible for telegram api and
-- appropriate handling of other high-level bot interactions (menu
-- output etc).
module FrontEnd.Telegram.Telegram where

import Prelude as P
--import Servant.API
import Servant.Client

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Data.IORef
import Data.Vector as V
import Data.Map as M
import Data.Set as S 

import Data.Semigroup
import Data.Maybe
import Control.Monad.Base
import Control.Applicative

import qualified Data.Text as T
import qualified Data.Text.Read as T
--import qualified Data.Text.IO as TIO
import qualified System.IO as SIO

import qualified Control.Monad.State.Lazy as SL

import qualified FrontEnd.Telegram.Data.GetUpdate as GU
import qualified FrontEnd.Telegram.Data.PollMessage as PM
import qualified FrontEnd.Telegram.API as API

import qualified EchoBot
import qualified Logger
import Logger ((.<))

data Handle = Handle
  { configTelegramBot :: Config
  , lastUpdate :: IORef (Maybe Int)
  , handleEchoBot :: EchoBot.Handle (SL.StateT Accounting ClientM) AccountMessage
  }

logHandle :: Handle -> Logger.Handle (SL.StateT Accounting ClientM)
logHandle = EchoBot.hLogHandle .  handleEchoBot -- error "Not implement"

type UserId = Int
type ChatId = Int
type PollId = T.Text

data Accounting = Accounting
  { currentAccountId :: AccountId
  , currentState :: EchoBot.State
  , currentPollID :: Maybe PollId
  , mapState :: Map UserId AccountState
  }

data AccountState = AccountState
  { accountId :: AccountId
  , accountState :: EchoBot.State
  , accountPollId :: Maybe PollId
  }

data AccountId = AccountId 
  { accountUserId :: UserId -- accountIdFitstNameUser :: Name
  , accountIdChatId :: ChatId
  } deriving Eq

data AccountMessage = AccountMessage
  { accountMesageText :: T.Text
  --, accountMesageID :: Int
  , accountEntitiesMessage :: Maybe (Vector GU.Entity )
  }
  | AccountMessagePhoto 
  { -- accountMesageID :: Int
  accountFileIDPhoto :: Set T.Text
  }

accountMessageToText :: AccountMessage -> Maybe T.Text
accountMessageToText (AccountMessage t _) = Just t
accountMessageToText _ = Nothing

textToAccountMessage :: T.Text -> AccountMessage
textToAccountMessage t = AccountMessage t Nothing

data AccountEvent = AccountEvent
  { accountChatId :: ChatId
  -- , accountFirstName :: Name
  , accountMessages :: Vector AccountMessage
  }

data AccountPoll = AccountPoll
  { accountUpdateIDPoll :: Int
  , accountIdPoll :: T.Text
  , accTotalVoterPoll :: Int
  , accOptionsPoll :: Vector GU.Option
  } deriving Show

data Config = Config
  { confBotToken :: T.Text -- confManager :: 
  , confFirstNameBot :: T.Text
  } deriving Show

withHandle :: Config 
           -> EchoBot.Handle (SL.StateT Accounting ClientM) AccountMessage
           -> (Handle -> SL.StateT Accounting ClientM a) 
           -> SL.StateT Accounting ClientM a
withHandle conf ebh f = do 
  lu <- liftBase $ newIORef Nothing
  f $ Handle 
    { configTelegramBot = conf
    , handleEchoBot = ebh
    , lastUpdate = lu
    }

run :: Handle -> SL.StateT Accounting ClientM ()
run h = do
  _ <- hGetUpdates h
  run' h

run' :: Handle -> SL.StateT Accounting ClientM ()
run' h = do
  runLoop h
  run' h

runLoop :: Handle -> SL.StateT Accounting ClientM () -- IO ()
runLoop h = do
  -- mlastUp <- liftBase $ readIORef (lastUpdate h)
  Logger.logInfo (logHandle h) "Start main telegram bot loop"
  vUp <- hGetUpdates h
  let (mapNAccount,mapAccPoll) = filterResult vUp
  _ <- P.sequence $ M.mapWithKey (\n-> (updateAccount h n)) mapNAccount
  -- refreshAccount h
  s <- SL.get
  _ <- P.sequence $ M.mapWithKey (\n-> (updatePoll h n mapAccPoll)) (mapState s)
  return ()
  where
    filterResult = vResultTovAccountEvent -- .
      -- filterOnlyUsers (configTelegramBot h)

hGetUpdates :: Handle -> SL.StateT Accounting ClientM (Vector GU.ResultElement)
hGetUpdates h = do
  mlastUp <- liftBase $ readIORef (lastUpdate h)
  Logger.logDebug (logHandle h) $ "Current last update " .< mlastUp
  case mlastUp of
    (Just lUp) -> do
      (GU.Welcome10 _ vUp) <- SL.lift $ API.getUpdates (Just (lUp + 1) )
      Logger.logDebug (logHandle h) $ "Last result vector length " .< (V.length vUp)
      Logger.logDebug (logHandle h) $ "Filtered result vector length " .< (V.length $ filterUpdate lUp vUp)
      Logger.logDebug (logHandle h) $ "Vector IDUpdates " .< (fmap GU.updateIDResultElement vUp )
      liftBase $ writeIORef (lastUpdate h) (Just $ max (getNewLastUpdate vUp) lUp )
      return $ filterUpdate lUp vUp
    _ -> do
      (GU.Welcome10 _ vUp) <- SL.lift $ API.getUpdates Nothing
      Logger.logDebug (logHandle h) $ "Last result vector length " .< (V.length vUp)
      liftBase $ writeIORef (lastUpdate h) (Just $ getNewLastUpdate vUp)
      return $ vUp
    
getNewLastUpdate :: Vector GU.ResultElement -> Int
getNewLastUpdate = getMax . P.foldMap (Max . GU.updateIDResultElement)

filterUpdate :: Int -> Vector GU.ResultElement -> Vector GU.ResultElement
filterUpdate lastUp = V.filter 
  (\re-> 
    (lastUp < (GU.updateIDResultElement re)) )

updateAccount :: Handle -> UserId -> AccountEvent -> SL.StateT Accounting ClientM ()
updateAccount h n (AccountEvent chatID vMessage) = do
  Logger.logDebug (logHandle h) $ "Current UserId " .< n
  switchAccount h n chatID 
  s1 <- SL.get
  Logger.logDebug (logHandle h) $ "Repetition count" .< (currentState s1)
  _ <- P.mapM (updateAccountMessage h chatID) (V.toList vMessage)
  refreshAccount h
  return ()
-- error "Not implement"
-- updateAccount h n (AccountPoll chatID accTVotes vOption) = do
--  switchAccount h n chatID 

updatePoll :: Handle -> UserId -> Map PollId AccountPoll -> AccountState -> SL.StateT Accounting ClientM ()
updatePoll h n mapNAP ast = do
  Logger.logDebug (logHandle h) $ "Update account poll " 
  Logger.logDebug (logHandle h) $ "Poll account " .< (accountPollId ast)
  Logger.logDebug (logHandle h) $ "Map polls" .< mapNAP
  case ((accountPollId ast) >>= (\i-> mapNAP M.!? i) ) of
    (Just p) -> if (accTotalVoterPoll p) > 0
      then do
        let mO = P.foldl1 maxOption (accOptionsPoll p)
        Logger.logDebug (logHandle h) $ "Current poll maximum for option " .< mO 
        switchAccount h n (accountIdChatId $ accountId ast)
        r <- EchoBot.respond 
          (handleEchoBot h) 
          (EchoBot.SetRepetitionCountEvent $ 
            either (const (EchoBot.stRepetitionCount $ accountState ast) ) fst $ 
            T.decimal $ GU.optionTextOption mO)
        P.mapM_ (sendAccMessage h (accountIdChatId $ accountId ast)) r
        SL.modify (\s->s{currentPollID = Nothing})
        s <- SL.get
        Logger.logDebug (logHandle h) $ "Repetition count modify" .< (currentState s)
        refreshAccount h
        Logger.logDebug (logHandle h) $ "Repetition count modify post \"refrashAccount\" " .< (currentState s)
        return ()
      else return ()
    _ -> return ()
  where
    maxOption o1 o2 
      | (GU.voterCountOption o1) >= (GU.voterCountOption o2) = o1
      | True = o2

refreshAccount :: Handle -> SL.StateT Accounting ClientM ()
refreshAccount h = do
  s <- SL.get
  switchAccount h (accountUserId $ currentAccountId s) (accountIdChatId $ currentAccountId s)

switchAccount :: Handle -> UserId -> Int -> SL.StateT Accounting ClientM ()
switchAccount h n chatId = do
  s <- SL.get
  -- if ((currentAccountId s) /= (AccountId n chatId) )
  case ((mapState s) M.!? n) of
    (Just accSt) -> do
       SL.modify (\s2->s2 {mapState = f (mapState s) (currentAccountId s) (currentState s) (currentPollID s) } )
       SL.modify (\s2->s2 
         { currentAccountId = AccountId n chatId
         , currentState = accountState accSt
         } )
    _ -> do
      SL.modify (\s2->s2 {mapState = f (mapState s) (currentAccountId s) (currentState s) (currentPollID s)} )
      let conf = EchoBot.hConfig $ handleEchoBot h
      SL.modify (\s2->s2 
         { currentAccountId = AccountId n chatId
         , currentState = EchoBot.State $ EchoBot.confRepetitionCount conf
         } )
    -- else return ()
  where
    f mapSt currentAccId currentSt currentPollID' = M.insert 
      (accountUserId currentAccId) 
      (AccountState currentAccId currentSt currentPollID')
      mapSt

updateAccountMessage :: Handle -> Int -> AccountMessage -> SL.StateT Accounting ClientM ()
updateAccountMessage h chatId x = do -- error "Not implement"
  lr <- EchoBot.respond (handleEchoBot h) (EchoBot.MessageEvent x)
  -- s1 <- SL.get
  -- Logger.logDebug (logHandle h) $ "Repetition count" .< (currentState s1)
  case lr of
    ((EchoBot.MenuResponse t lre):_ ) -> do
      (PM.Welcome9 _ r) <- SL.lift $ API.sendPoll
        (Just chatId) (Just t) (Just $ printToListText lre)
      Logger.logDebug (logHandle h) $ "Send Poll "
      SL.modify (\s->s{currentPollID = Just $ PM.pollIDPoll $ PM.pollResultClass $ r } )
    ys -> do 
      Logger.logDebug (logHandle h) $ "Output repeat message " .< (P.length ys)
      P.mapM_ (sendAccMessage h chatId) ys
  where
    printToListText :: [(EchoBot.RepetitionCount, EchoBot.Event a)] -> API.ListText
    printToListText [] = API.ListText []
    printToListText ((y,_):ys) = API.ListText $ (\lt-> (T.pack $ show y) :lt) $ API.unListText $ printToListText ys

sendAccMessage :: Handle -> Int ->  EchoBot.Response AccountMessage -> SL.StateT Accounting ClientM ()
sendAccMessage _ chatId (EchoBot.MessageResponse (AccountMessage t _ ) ) = do
  _ <- SL.lift $ API.sendMessage (Just chatId) (Just t)
  -- liftBase $ modifyIORef (lastUpdate h) (fmap ((+) 1) )
  return ()
sendAccMessage h chatId (EchoBot.MessageResponse (AccountMessagePhoto sphid ) ) = do
  Logger.logDebug (logHandle h) $ "AccountMessagePhoto to list " .< (P.length (S.elems sphid))
  _ <- SL.lift $ API.sendPhoto (Just chatId) (Just $ S.findMax sphid) -- P.mapM (P.foldl f [] sphid)
  -- liftBase $ modifyIORef (lastUpdate h) (fmap ((+) 1) ) 
  return ()
{-  where
    f [] p = [p]
    f (x:l) p | x /= p = p:x:l 
    f l _ = l
-}
sendAccMessage _ _ _ = return ()

filterOnlyUsers :: Config -> Vector GU.ResultElement -> Vector GU.ResultElement
filterOnlyUsers conf = V.filter f
  where
    f r = maybe False id $ do
      mre <- GU.messageResultElement r
      return $ (confFirstNameBot conf) /= (GU.firstNameFrom $ GU.fromMessage mre)

vResultTovAccountEvent :: Vector GU.ResultElement -> (Map UserId AccountEvent, Map PollId AccountPoll) -- Maybe (Map Name [AccountEvent]) -- AccountMessage 
vResultTovAccountEvent = P.foldl f (M.empty,M.empty)
  where
    f (ma,mapoll) mre = fromJust $ (do
      re <- GU.messageResultElement mre 
      accMessage <- readAccountMessage re
      return (M.insertWith g
        (GU.fromIDFrom $ GU.fromMessage re) 
        (AccountEvent 
          { accountChatId = GU.chatIDChat $ GU.chatMessage re
          , accountMessages = V.singleton $ accMessage
          }
        ) ma, mapoll) 
      ) <|> ( do
      re <- GU.pollResultElement  mre
      return (ma , M.insertWith accomulatePoll
        (GU.pollIDPoll re) 
        ( AccountPoll 
          { accountUpdateIDPoll = GU.updateIDResultElement mre
          , accountIdPoll = GU.pollIDPoll re
          , accTotalVoterPoll = GU.totalVoterCountPoll re
          , accOptionsPoll = GU.optionsPoll re
          }
        ) mapoll)
      ) <|> (return (ma,mapoll))
    readAccountMessage 
      (GU.Message _ _ _ _ _ _ (Just vphoto)) = Just $ AccountMessagePhoto 
        { -- accountMesageID = mid
        accountFileIDPhoto = P.foldl (\s ph -> S.insert (GU.fileIDPhoto ph) s) (S.empty) vphoto
        }
    readAccountMessage 
      (GU.Message _ _ _ _ (Just mText) mve _ ) = Just $ AccountMessage
        { accountMesageText = mText
        -- , accountMesageID = mid
        , accountEntitiesMessage = mve
        }
    readAccountMessage _ = Nothing
    g (AccountEvent _ m1) (AccountEvent chatid m2) = AccountEvent chatid (m1 V.++ m2)
    accomulatePoll a1@(AccountPoll rid _ _ _) a2@(AccountPoll rid2 _ _ _)
      | rid > rid2 = a1
      | True = a2

clientEnvDefault :: Config -> IO ClientEnv
clientEnvDefault conf = do 
  m <- newManager tlsManagerSettings -- defaultManagerSettings
  url <- parseBaseUrl $ "https://api.telegram.org/bot" P.++ (T.unpack $ confBotToken conf)
  return $ mkClientEnv m url

testUpdate' :: Config -> IO () -- (Either ClientError Data.Welcome10)
testUpdate' conf = do
  m <- clientEnvDefault conf
  e <- runClientM (do
    API.getUpdates (Just 1)
    ) m
  SIO.print e 
