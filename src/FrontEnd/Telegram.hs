{-# LANGUAGE OverloadedStrings #-}

-- | The console front-end is responsible for console I/O and
-- appropriate handling of other high-level bot interactions (menu
-- output etc).
module FrontEnd.Telegram
  ( run,
    Handle (..),
  )
where

import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.IO as TIO
import qualified EchoBot


