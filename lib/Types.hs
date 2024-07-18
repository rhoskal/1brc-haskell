module Types where

import RIO
import RIO.PrettyPrint (HasStylesUpdate (..), HasTerm (..))
import RIO.PrettyPrint.StylesUpdate (StylesUpdate (..))
import RIO.Process

-- | Command line arguments
data AppOptions = AppOptions
  { aoDebug :: !Bool,
    aoFilePath :: !FilePath
  }
  deriving (Show)

data App = App
  { appLogFn :: !LogFunc,
    appOptions :: !AppOptions,
    appProcessContext :: !ProcessContext,
    appUseColor :: !Bool,
    appTermWidth :: !Int,
    appStylesUpdate :: !StylesUpdate
  }

instance HasLogFunc App where
  logFuncL = lens appLogFn (\x y -> x {appLogFn = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})

class HasAppOptions env where
  appOptionsL :: Lens' env AppOptions

instance HasAppOptions App where
  appOptionsL = lens appOptions (\x y -> x {appOptions = y})

instance HasStylesUpdate App where
  stylesUpdateL = lens appStylesUpdate (\x y -> x {appStylesUpdate = y})

instance HasTerm App where
  useColorL = lens appUseColor (\x y -> x {appUseColor = y})
  termWidthL = lens appTermWidth (\x y -> x {appTermWidth = y})
