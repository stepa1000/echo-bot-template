{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ConfigurationTypes
  ( FrontEndType (..),
    textToFrontEndType,
  )
where

import Data.Text as T
import Data.Yaml
import GHC.Generics

data FrontEndType
  = ConsoleFrontEnd
  | TelegramFrontEnd
  deriving (Generic, ToJSON, FromJSON)

textToFrontEndType :: Text -> FrontEndType
textToFrontEndType t
  | t == "Console" = ConsoleFrontEnd
  | t == "Telegram" = TelegramFrontEnd
  | otherwise = ConsoleFrontEnd
