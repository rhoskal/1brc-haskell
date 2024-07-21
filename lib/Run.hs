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
import Text.Printf
import Types
import Prelude (putStrLn, readFile, writeFile)

data Statistics = Statistics
  { cMin :: !Float,
    cMean :: !Float,
    cMax :: !Float
  }
  deriving (Eq)

toString :: Statistics -> String
toString stat = printf "%f/%f/%f" (cMin stat) (cMean stat) (cMax stat)

roundTowardPositive :: (RealFrac a) => a -> Float
roundTowardPositive x = fromIntegral ((ceiling $ x * 10) :: Integer) / 10.0 :: Float

meanMaybe :: (Fractional a) => [a] -> Maybe a
meanMaybe xs
  | null xs = Nothing
  | otherwise = Just $ List.sum xs / (List.genericLength xs)

calcStatistics :: [Celsius] -> Statistics
calcStatistics cs =
  let min' :: [Celsius] -> Float
      min' = maybe (0.0 :: Float) unCelsius . List.minimumMaybe

      mean' :: [Celsius] -> Float
      mean' = roundTowardPositive . maybe (0.0 :: Float) unCelsius . meanMaybe

      max' :: [Celsius] -> Float
      max' = maybe (0.0 :: Float) unCelsius . List.maximumMaybe
   in Statistics (min' cs) (mean' cs) (max' cs)

buildFinalStr :: Map Station [Celsius] -> String
buildFinalStr ms = do
  let str =
        List.intercalate ", "
          $ List.map (\(Station station, temps) -> station <> "=" <> (toString $ calcStatistics temps))
          $ Map.toList ms
   in "{" <> str <> "}\n"

writeToStdout :: String -> IO ()
writeToStdout = putStrLn

writeToFile :: FilePath -> String -> IO ()
writeToFile filePath content = writeFile filePath content

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
    Nothing -> liftIO $ writeToStdout $ buildFinalStr aggregated
    Just outFilePath -> liftIO $ writeToFile outFilePath $ buildFinalStr aggregated
