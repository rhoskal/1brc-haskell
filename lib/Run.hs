module Run (run) where

import Parser
import RIO
import RIO.ByteString qualified as B
import RIO.List (intersperse)
import RIO.Map qualified as Map
import RIO.PrettyPrint qualified as PP
import Summary
import Types

type Accumulator = Map Station Summary

strBuilder :: Accumulator -> ByteString
strBuilder !acc =
  let buildEntry :: (Station, Summary) -> ByteString
      buildEntry (Station !station, !summary) =
        station
          <> B.singleton 61 -- c2w '=' == 61
          <> formatSummary summary
   in B.singleton 123 -- c2w '{' == 123
        <> mconcat (intersperse (B.pack [44, 32]) (map buildEntry $ Map.toList acc))
        <> B.singleton 125 -- c2w '}' == 125
        <> B.singleton 10 -- c2w '\n' == 10

addObservation :: Accumulator -> Observation -> Accumulator
addObservation !acc !o =
  let !station = oStation o
      !summary = mkInitialSummary $ oCelsius o
   in Map.insertWith mergeSummary station summary acc

printResults :: Accumulator -> IO ()
printResults !acc = B.putStr $ strBuilder acc

lines' :: ByteString -> [ByteString]
lines' !ps
  | B.null ps = []
  | otherwise = case search ps of
      Nothing -> [ps]
      Just !n -> B.take n ps : lines' (B.drop (n + 1) ps)
  where
    search = B.elemIndex 10 -- c2w '\n' == 10

run :: RIO App ()
run = do
  env <- ask
  logDebug "Running v5..."
  contents <- B.readFile (aoInputFilePath $ view appOptionsL env)
  let !observations = map unsafeParse $ lines' contents
  let !processed = foldl' addObservation Map.empty observations
  logDebug
    =<< PP.displayWithColor
      ( PP.flow "First 10 parsed observations:"
          <> PP.line
          <> PP.bulletedList (take 10 $ map (fromString . show) observations)
      )
  logDebug
    =<< PP.displayWithColor
      ( PP.flow "First 10 processed:"
          <> PP.line
          <> PP.bulletedList (take 10 $ map (fromString . show) $ Map.toList processed)
      )
  liftIO $ printResults processed
