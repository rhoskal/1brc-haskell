module Run (run) where

import Control.Monad.State
  ( State,
    execState,
    modify,
  )
import Parser
import RIO
import RIO.List qualified as List
import RIO.Map qualified as Map
import RIO.PrettyPrint qualified as P
import Types
import Prelude (putStrLn, readFile)

data Statistics = Statistics
  { cMin :: !Float,
    cMean :: !Float,
    cMax :: !Float
  }
  deriving (Eq)

instance Show Statistics where
  show c = List.intercalate "/" $ map show [cMin c, cMean c, cMax c]

meanMaybe :: (Fractional a) => [a] -> Maybe a
meanMaybe xs
  | null xs = Nothing
  | otherwise = Just $ List.sum xs / (List.genericLength xs)

calcStatistics :: [Celsius] -> Statistics
calcStatistics cs =
  let min' :: [Celsius] -> Float
      min' = maybe (0.0 :: Float) unCelsius . List.minimumMaybe

      mean' :: [Celsius] -> Float
      mean' = maybe (0.0 :: Float) unCelsius . meanMaybe

      max' :: [Celsius] -> Float
      max' = maybe (0.0 :: Float) unCelsius . List.maximumMaybe
   in Statistics (min' cs) (mean' cs) (max' cs)

writeToStdout :: Map Station [Celsius] -> IO ()
writeToStdout ms = do
  let cs = List.map (\(station, cs') -> (station, calcStatistics cs')) (Map.toList ms)
      str =
        List.foldl
          ( \acc (station, stats) ->
              concat
                [ acc,
                  (show $ unStation station),
                  "=",
                  show stats,
                  ", "
                ]
          )
          ""
          cs
   in putStrLn $ "{" ++ str ++ "}"

writeToFile :: FilePath -> Map Station [Celsius] -> IO ()
writeToFile _ _ = putStrLn "no implemented yet"

addMeasurement :: Measurement -> State (Map Station [Celsius]) ()
addMeasurement m = modify $ Map.insertWith (++) (mStation m) (mCelsius m : [])

run :: RIO App ()
run = do
  env <- ask
  logDebug "Running v0 (naive)..."
  content <- liftIO $ readFile $ aoInputFilePath $ view appOptionsL env
  let parsed = catMaybes $ map parser $ lines content
  let aggregated = execState (mapM_ addMeasurement parsed) Map.empty
  logDebug
    =<< P.displayWithColor
      ( P.flow "First 10 measurements:"
          <> P.line
          <> P.bulletedList (take 10 $ map (fromString . show) parsed)
      )
  case aoOutputFilePath $ view appOptionsL env of
    Nothing -> liftIO $ writeToStdout aggregated
    Just outFilePath -> liftIO $ writeToFile outFilePath aggregated
