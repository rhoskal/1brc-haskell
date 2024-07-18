module Types where

import RIO
import RIO.Process

-- | Command line arguments
data AppOptions = AppOptions
  { aoDebug :: !Bool,
    aoFilePath :: !FilePath,
    aoImpl :: !Text
  }
  deriving (Show)

data Impl
  = V0
  | V1
  deriving (Eq, Show)

data App = App
  { appLogFn :: !LogFunc,
    appOptions :: !AppOptions,
    appProcessContext :: !ProcessContext
  }

instance HasLogFunc App where
  logFuncL = lens appLogFn (\x y -> x {appLogFn = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})

class HasAppOptions env where
  appOptionsL :: Lens' env AppOptions

instance HasAppOptions App where
  appOptionsL = lens appOptions (\x y -> x {appOptions = y})
