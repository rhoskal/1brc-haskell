module Types where

import RIO
import RIO.Process

-- | Command line arguments
newtype AppOptions = AppOptions
  { aoDebug :: Bool
  }
  deriving (Show)

data App = App
  { appLogFn :: !LogFunc,
    appOptions :: !AppOptions,
    appProcessContext :: !ProcessContext
  }

instance HasLogFunc App where
  logFuncL = lens appLogFn (\x y -> x {appLogFn = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})
