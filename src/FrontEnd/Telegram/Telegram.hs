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
--import qualified Data.Text.Read as T
--import qualified Data.Text.IO as TIO
import qualified System.IO as SIO

import qualified Control.Monad.State.Lazy as SL

import qualified FrontEnd.Telegram.Data.GetUpdate as GU
import qualified FrontEnd.Telegram.Data.PollMessage as PM
import qualified FrontEnd.Telegram.API as API
import qualified EchoBot

data Handle = Hendle
  { configTelegramBot :: Config
  , lastUpdate :: IORef (Maybe Int)
  , handleEchoBot :: EchoBot.Handle (SL.StateT Accounting ClientM) AccountMessage
  }

type Name = T.Text
type PollID = T.Text

data Accounting = Accounting
  { currentAccountId :: AccountId
  , currentState :: EchoBot.State
  , currentPollID :: Maybe PollID
  , mapState :: Map Name AccountState
  }

data AccountState = AccountState
  { accountId :: AccountId
  , accountState :: EchoBot.State
  , accountPollId :: Maybe PollID
  }

data AccountId = AccountId 
  { accountIdFitstNameUser :: Name
  , accountIdChatId :: Int
  }

data AccountMessage = AccountMessage
  { accountMesageText :: T.Text
  , accountMesageID :: Int
  , accountEntitiesMessage :: Maybe (Vector GU.Entity )
  }
  | AccountMessagePhoto 
  { accountMesageID :: Int
  , accountFileIDPhoto :: Set T.Text
  }

data AccountEvent = AccountEvent
  { accountChatId :: Int
  -- , accountFirstName :: Name
  , accountMessages :: Vector AccountMessage
  }

data AccountPoll = AccountPoll
  { accountIdPoll :: T.Text
  , accTotalVoterPoll :: Int
  , accOptionsPoll :: Vector GU.Option
  }

data Config = Config
  { confBotToken :: T.Text -- confManager :: 
  } deriving Show

run :: Handle -> SL.StateT Accounting ClientM () -- IO ()
run h = do
  -- mlastUp <- liftBase $ readIORef (lastUpdate h)
  vUp <- hGetUpdates h
  let (mapNAccount,mapAccPoll) = vResultTovAccountEvent vUp
  mapNA2 <- P.sequence $ M.mapWithKey (\n-> (updateAccount h n)) mapNAccount
  return ()

hGetUpdates :: Handle -> SL.StateT Accounting ClientM (Vector GU.ResultElement)
hGetUpdates h = do
  mlastUp <- liftBase $ readIORef (lastUpdate h)
  case mlastUp of
    (Just lUp) -> do
      (GU.Welcome10 _ vUp) <- SL.lift $ API.getUpdates (Just lUp)
      liftBase $ writeIORef (lastUpdate h) (Just $ getNewLastUpdate vUp)
      return $ filterUpdate lUp vUp
    _ -> do
      (GU.Welcome10 _ vUp) <- SL.lift $ API.getUpdates Nothing
      liftBase $ writeIORef (lastUpdate h) (Just $ getNewLastUpdate vUp)
      return $ vUp
    
getNewLastUpdate :: Vector GU.ResultElement -> Int
getNewLastUpdate = getMax . P.foldMap (Max . GU.updateIDResultElement)

filterUpdate :: Int -> Vector GU.ResultElement -> Vector GU.ResultElement
filterUpdate lastUp = V.filter 
  (\re-> 
    (isJust $ GU.messageResultElement re) && 
    (lastUp < (GU.dateMessage $ fromJust $ GU.messageResultElement re)) )

updateAccount :: Handle -> Name -> AccountEvent -> SL.StateT Accounting ClientM ()
updateAccount h n (AccountEvent chatID vMessage) = do
  switchAccount h n chatID 
  P.mapM (updateAccountMessage h n chatID) (V.toList vMessage)
  error "Not implement"
-- updateAccount h n (AccountPoll chatID accTVotes vOption) = do
--  switchAccount h n chatID 

switchAccount :: Handle -> Name -> Int -> SL.StateT Accounting ClientM ()
switchAccount h n chatId = do
  s <- SL.get
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
  where
    f mapSt currentAccId currentSt currentPollID' = M.insert 
      (accountIdFitstNameUser currentAccId) 
      (AccountState currentAccId currentSt currentPollID')
      mapSt

updateAccountMessage :: Handle -> Name -> Int -> AccountMessage -> SL.StateT Accounting ClientM ()
updateAccountMessage h n chatId x = do -- error "Not implement"
  lr <- EchoBot.respond (handleEchoBot h) (EchoBot.MessageEvent x)
  case lr of
    ((EchoBot.MenuResponse t lre):_ ) -> do
      (PM.Welcome9 _ r) <- SL.lift $ API.sendPoll
        (Just chatId) (Just t) (Just $ printToListText lre)
      SL.modify (\s->s{currentPollID = Just $ PM.pollIDPoll $ PM.pollResultClass $ r } )
    ys -> P.mapM_ sendAccMessage ys
  where
    sendAccMessage :: EchoBot.Response AccountMessage -> SL.StateT Accounting ClientM ()
    sendAccMessage (EchoBot.MessageResponse (AccountMessage t idm me ) ) = do
      SL.lift $ API.sendMessage (Just chatId) (Just t)
      return ()
    sendAccMessage (EchoBot.MessageResponse (AccountMessagePhoto dm sphid ) ) = do
      SL.lift $ P.mapM (API.sendPhoto (Just chatId) . Just) (S.toList sphid)
      return ()
    printToListText :: [(EchoBot.RepetitionCount, EchoBot.Event a)] -> API.ListText
    printToListText ((x,_):xs) = API.ListText $ (\lt-> (T.pack $ show x) :lt) $ API.unListText $ printToListText xs

vResultTovAccountEvent :: Vector GU.ResultElement -> (Map Name AccountEvent, Map Name [AccountPoll]) -- Maybe (Map Name [AccountEvent]) -- AccountMessage 
vResultTovAccountEvent = P.foldl f (M.empty,M.empty)
  where
    f (ma,mapoll) mre = fromJust $ (do
      re <- GU.messageResultElement mre 
      return (M.insertWith g
        (GU.firstNameFrom $ GU.fromMessage re) 
        (AccountEvent 
          { accountChatId = GU.chatIDChat $ GU.chatMessage re
          , accountMessages = V.singleton $ readAccountMessage re
          }
        ) ma, mapoll) 
      ) <|> ( do
      re <- GU.pollResultElement  mre
      return (ma , M.insertWith (P.++)
        (GU.pollIDPoll re) 
        [( AccountPoll 
          { accountIdPoll = GU.pollIDPoll re
          , accTotalVoterPoll = GU.totalVoterCountPoll re
          , accOptionsPoll = GU.optionsPoll re
          }
        )] mapoll)
      )
    readAccountMessage 
      (GU.Message mid _ _ _ _ _ (Just vphoto)) = AccountMessagePhoto 
        { accountMesageID = mid
        , accountFileIDPhoto = P.foldl (\s ph -> S.insert (GU.fileIDPhoto ph) s) (S.empty) vphoto
        }
    readAccountMessage 
      (GU.Message mid _ _ _ mText mve _ ) = AccountMessage
        { accountMesageText = mText
        , accountMesageID = mid
        , accountEntitiesMessage = mve
        }
    g (AccountEvent _ m1) (AccountEvent chatid m2) = AccountEvent chatid (m1 V.++ m2)

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
