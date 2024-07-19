module Run (run) where

import Parser (parser)
import RIO
import RIO.PrettyPrint qualified as P
import Types
import Prelude (readFile)

run :: RIO App ()
run = do
  env <- ask
  logDebug "Running v0 (naive)..."
  let parsed = map parser $ lines content
  content <- liftIO $ readFile $ aoInputFilePath $ view appOptionsL env
  logDebug
    =<< P.displayWithColor
      ( P.flow "First 10 measurements:"
          <> P.line
          <> P.bulletedList
            ( take 10
                $ map (maybe "Unable to parse Measurement" (fromString . show)) parsed
            )
      )
