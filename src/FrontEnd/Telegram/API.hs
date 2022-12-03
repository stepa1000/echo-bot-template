{-# Language TypeOperators ,
DataKinds 
#-}

module FrontEnd.Telegram.API where

-- import Servant.API
-- import Servant.Client

-- import Data.Proxy
import Network.HTTP.Client
-- import Network.HTTP.Types.Status

import FrontEnd.Telegram.Data.GetUpdate as GU
-- import FrontEnd.Telegram.Data.SendMessage as SM

type BaseUrl = String
-- type Token = String

getUpdate :: BaseUrl -> Manager -> Int -> IO (Maybe Welcome10) -- (Either String Welcome10)
getUpdate burl m i = do
  r <- parseRequest $ burl ++ "/getUpdates?offset=" ++ (show i)
  response <- httpLbs r m
  return $ GU.decodeTopLevel $ responseBody response
{-  withResponse r m $ \ response -> do
    status <- responseStatus response
    if status == ok200
       then do
         fmap (maybe (Left "decode error in getUpdate") Right) (responseBody response >>= GU.decodeTopLevel)
       else return $ Left (show status)
-}  
{-
type TelegramAPI = "getUpdates" :> QueryParam "offset" Int :> Put '[JSON] Welcome10
              :<|> "sendMessage" :> QueryParam "chat_id" Int :> Put '[JSON] Welcome2
 (QueryParam "chat_id" Int 
                              :<|> QueryParam "chat_id" String )

telegramAPI :: Proxy TelegramAPI
telegramAPI = Proxy

getUpdates :: Maybe Int -> ClientM Welcome10
sendMessage :: Maybe Int -> ClientM Welcome2

getUpdates :<|> sendMessage = client telegramAPI
-}
