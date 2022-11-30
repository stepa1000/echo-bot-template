{-# LANGUAGE OverloadedStrings #-}

-- | The default implementation of the Logger interface.
module Logger.Impl
  ( withHandle,
    Config (..),
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Logger
import qualified System.IO

data Config = Config
  { -- | A file handle to output formatted log messages to with
    -- 'System.IO.hPutStrLn' or 'Data.Text.IO.hPutStrLn'. For example,
    -- it might be 'System.IO.stderr' or a handle of a regular open
    -- file.
    confFileHandle :: System.IO.Handle,
    -- | The minimum level of a printable log message. Messages with
    -- lower levels should not be printed.
    confMinLevel :: Logger.Level
  }

withHandle :: Config -> (Logger.Handle IO -> IO ()) -> IO ()
withHandle config f = f Logger.Handle {Logger.hLowLevelLog = logWith config}

logWith :: Config -> Logger.Level -> T.Text -> IO ()
logWith conf logLvl t | logLvl >= (confMinLevel conf) = do
  T.hPutStrLn (confFileHandle conf) $ (T.pack $ show logLvl ++ ": " ) `T.append` t
logWith _ _ _ = return ()

