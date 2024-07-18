module Run (run) where

import Parser (parser)
import RIO
import Types
import Prelude (putStrLn, readFile)

run :: RIO App ()
run = do
  env <- ask
  logDebug "Running v0 (naive)..."
  content <- liftIO $ readFile $ aoFilePath $ view appOptionsL env
  let parsed = map parser (lines content)
   in liftIO $ putStrLn $ show parsed
