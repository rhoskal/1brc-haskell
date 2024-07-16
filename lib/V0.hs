module V0 (run) where

import RIO
import Types

run :: RIO App ()
run = do
  logDebug "Running v0 (naive)..."
