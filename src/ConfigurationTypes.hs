{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module ConfigurationTypes
  ( FrontEndType (..),
  textToFrontEndType
  )
where

import GHC.Generics

import Data.Text as T

import Data.Yaml

data FrontEndType
  = ConsoleFrontEnd
  | TelegramFrontEnd
  deriving (Generic, ToJSON, FromJSON)

textToFrontEndType :: Text -> FrontEndType
textToFrontEndType t
  | t == "Console" = ConsoleFrontEnd
  | t == "Telegram" = TelegramFrontEnd
  | otherwise = ConsoleFrontEnd
