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

import qualified Data.Text as T
--import qualified Data.Text.Read as T
--import qualified Data.Text.IO as TIO
import qualified System.IO as SIO

import qualified Control.Monad.State.Lazy as SL

import qualified FrontEnd.Telegram.Data.GetUpdate as GU
import qualified FrontEnd.Telegram.API as API
import qualified EchoBot

data Handle = Hendle
  { configTelegramBot :: Config
  , lastUpdate :: IORef (Maybe Int)
  , handleEchoBot :: EchoBot.Handle (SL.StateT Accounting ClientM) AccountMessage
  }

type Name = T.Text

data Accounting = Accounting
  { currentAccountId :: AccountId
  , currentState :: EchoBot.State
  , mapState :: Map Name AccountState
  }

data AccountState = AccountState
  { accountId :: AccountId
  , accountState :: EchoBot.State
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

data Config = Config
  { confBotToken :: T.Text -- confManager :: 
  } deriving Show

run :: Handle -> SL.StateT Accounting ClientM () -- IO ()
run h = do
  mlastUp <- liftBase $ readIORef (lastUpdate h)
  vUp <- hGetUpdates h
  let mapNAccount = vResultTovAccountEvent vUp
  mapNA2 <- M.mapMWithKey updateAccount mapNAccount
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
filterUpdate lastUp = V.filter (\re-> lastUp < (GU.dateMessage $ GU.messageResultElement re) )

updateAccount :: Name -> AccountEvent -> StateT Accounting ClientM ()
updateAccount n (AccountEvent chatID vMessage) = do
  switchAccount n chatID 
  updateAccountMessage (V.toList vMessage)
  error "Not implement"

switchAccount :: Name -> Int -> StateT Accounting ClientM ()
switchAccount n chatId = do
  s <- SL.get
  case (mapState s) M.!? n of
    (Just accSt) -> do
       modify (\s2->s2 {mapState = f (mapState s) (currentAccountId s) (currentState s) } )
  where
    f mapSt currentAccId currentSt = M.insert 
      (accountIdFitstNameUser currentAccId) 
      (AccountState currentAccId currentSt)
      mapSt

vResultTovAccountEvent :: Vector GU.ResultElement -> Map Name AccountEvent -- AccountMessage 
vResultTovAccountEvent = P.foldl f M.empty
  where
    f ma re = M.insertWith g 
      (GU.firstNameFrom $ GU.fromMessage $ GU.messageResultElement re) 
      (AccountEvent 
        { accountChatId = GU.chatIDChat $ GU.chatMessage $ GU.messageResultElement re
        , accountMessages = V.singleton $ readAccountMessage $ GU.messageResultElement re
        }
      ) ma
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
