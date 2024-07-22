module Run (run) where

import Control.Monad.State
  ( State,
    execState,
    modify,
  )
import Parser
import RIO
import RIO.ByteString qualified as B
import RIO.List qualified as List
import RIO.Map qualified as Map
import RIO.PrettyPrint qualified as P
import RIO.Text qualified as T
import Statistics qualified as S
import Types

convertToStr :: Map Station [Celsius] -> Text
convertToStr ms = T.singleton '{' <> str <> T.singleton '}' <> T.singleton '\n'
  where
    str :: Text
    str =
      T.intercalate ", "
        $ List.map
          ( \(Station station, cs) ->
              station
                <> (T.singleton '=')
                <> (S.toString $ S.mkStatistics cs)
          )
        $ Map.toList ms

addMeasurement :: Measurement -> State (Map Station [Celsius]) ()
addMeasurement m = modify $ Map.insertWith (++) (mStation m) (mCelsius m : [])

parseFile :: Text -> [Measurement]
parseFile = mapMaybe parser . T.lines

run :: RIO App ()
run = do
  env <- ask
  logDebug "Running v1..."
  bsContent <- B.readFile $ aoInputFilePath $ view appOptionsL env
  let textContent = T.decodeUtf8With T.lenientDecode bsContent
  let measurements = parseFile textContent
  logDebug
    =<< P.displayWithColor
      ( P.flow "First 10 parsed measurements:"
          <> P.line
          <> P.bulletedList (take 10 $ map (fromString . show) measurements)
      )
  let aggregated = execState (mapM_ addMeasurement measurements) Map.empty
  B.putStr $ T.encodeUtf8 $ convertToStr aggregated
