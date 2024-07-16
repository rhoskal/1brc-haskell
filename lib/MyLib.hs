module MyLib (runV0) where

import RIO
import Types

runV0 :: RIO App ()
runV0 = do
  logDebug "Running v0 (naive)..."
