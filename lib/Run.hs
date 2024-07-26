module Run (run) where

import Parser
import RIO
import RIO.ByteString qualified as B
import RIO.ByteString qualified as BL
import RIO.List qualified as List
import RIO.Map qualified as Map
import RIO.PrettyPrint qualified as PP
import RIO.Text qualified as T
import Summary
import Types

type Accumulator = Map Station Summary

strBuilder :: Accumulator -> Text
strBuilder !acc =
  let buildEntry :: (Station, Summary) -> Text
      buildEntry (Station !station, !summary) =
        station
          <> T.singleton '='
          <> formatSummary summary
   in T.singleton '{'
        <> mconcat (List.intersperse (T.pack ", ") (map buildEntry $ Map.toList acc))
        <> T.singleton '}'
        <> T.singleton '\n'

addObservation :: Accumulator -> Observation -> Accumulator
addObservation !acc !o =
  let !station = oStation o
      !summary = mkInitialSummary $ oCelsius o
   in Map.insertWith mergeSummary station summary acc

parseFile :: Text -> [Observation]
parseFile = mapMaybe unsafeParse . T.lines

run :: RIO App ()
run = do
  env <- ask
  logDebug "Running v4..."
  dataset <-
    T.decodeUtf8With T.lenientDecode
      <$> BL.readFile (aoInputFilePath $ view appOptionsL env)
  let !observations = parseFile dataset
  let !aggregated = List.foldl' addObservation Map.empty observations
  logDebug
    =<< PP.displayWithColor
      ( PP.flow "First 10 parsed observations:"
          <> PP.line
          <> PP.bulletedList (take 10 $ map (fromString . show) observations)
      )
  logDebug
    =<< PP.displayWithColor
      ( PP.flow "First 10 aggegrated:"
          <> PP.line
          <> PP.bulletedList (take 10 $ map (fromString . show) $ Map.toList aggregated)
      )
  B.putStr $ T.encodeUtf8 $ strBuilder aggregated
