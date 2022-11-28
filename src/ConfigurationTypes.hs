{-# LANGUAGE OverloadedStrings #-}
module ConfigurationTypes
  ( FrontEndType (..),
  textToFrontEndType
  )
where

import Data.Text as T

data FrontEndType
  = ConsoleFrontEnd
  | TelegramFrontEnd

textToFrontEndType :: Text -> FrontEndType
textToFrontEndType t
  | t == "Console" = ConsoleFrontEnd
  | t == "Telegram" = TelegramFrontEnd
  | True = ConsoleFrontEnd
