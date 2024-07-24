module Run (run) where

import Control.Monad.State
  ( State,
    execState,
    modify,
  )
import Parser
import RIO
import RIO.ByteString qualified as B
import RIO.ByteString qualified as BL
import RIO.List qualified as List
import RIO.Map qualified as Map
import RIO.PrettyPrint qualified as P
import RIO.Text qualified as T
import Summary
import Types

type Accumulator = Map Station Summary

convertToStr :: Accumulator -> Text
convertToStr !acc = T.singleton '{' <> str <> T.singleton '}' <> T.singleton '\n'
  where
    str :: Text
    !str =
      T.intercalate ", "
        $ List.map
          ( \(Station !station, !summary) ->
              station
                <> (T.singleton '=')
                <> (formatSummary summary)
          )
        $ Map.toList acc

addObservation :: Observation -> State Accumulator ()
addObservation !o =
  modify
    $ Map.insertWith mergeSummary (oStation o) (mkInitialSummary $ oCelsius o)

parseFile :: Text -> [Observation]
parseFile = mapMaybe parser . T.lines

run :: RIO App ()
run = do
  env <- ask
  logDebug "Running v2..."
  bsDataset <- BL.readFile $ aoInputFilePath $ view appOptionsL env
  let !textDataset = T.decodeUtf8With T.lenientDecode bsDataset
  let !observations = parseFile textDataset
  logDebug
    =<< P.displayWithColor
      ( P.flow "First 10 parsed observations:"
          <> P.line
          <> P.bulletedList (take 10 $ map (fromString . show) observations)
      )
  let !aggregated = execState (mapM_ addObservation observations) Map.empty
  logDebug
    =<< P.displayWithColor
      ( P.flow "First 10 aggegrated:"
          <> P.line
          <> P.bulletedList (take 10 $ map (fromString . show) (Map.toList aggregated))
      )
  B.putStr $ T.encodeUtf8 $ convertToStr aggregated
