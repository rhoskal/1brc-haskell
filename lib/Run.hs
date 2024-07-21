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
import Prelude (putStrLn, readFile)

data Statistics = Statistics
  { sMin :: !Float,
    sMean :: !Float,
    sMax :: !Float
  }

toString :: Statistics -> String
toString stat = printf "%f/%f/%f" (sMin stat) (sMean stat) (sMax stat)

calcStatistics :: [Celsius] -> Statistics
calcStatistics cs =
  let min' :: [Celsius] -> Float
      min' = maybe (0.0 :: Float) unCelsius . List.minimumMaybe

      mean' :: [Celsius] -> Float
      mean' = roundTowardPositive . maybe (0.0 :: Float) unCelsius . meanMaybe

      max' :: [Celsius] -> Float
      max' = maybe (0.0 :: Float) unCelsius . List.maximumMaybe

      meanMaybe :: (Fractional a) => [a] -> Maybe a
      meanMaybe xs
        | null xs = Nothing
        | otherwise = Just $ List.sum xs / (List.genericLength xs)

      roundTowardPositive :: (RealFrac a) => a -> Float
      roundTowardPositive n = fromIntegral ((round $ n * 10) :: Integer) / 10.0 :: Float
   in Statistics (min' cs) (mean' cs) (max' cs)

buildFinalStr :: Map Station [Celsius] -> String
buildFinalStr ms =
  let str =
        List.intercalate ", "
          $ List.map (\(Station station, cs) -> station <> "=" <> (toString $ calcStatistics cs))
          $ Map.toList ms
   in "{" <> str <> "}"

addMeasurement :: Measurement -> State (Map Station [Celsius]) ()
addMeasurement m = modify $ Map.insertWith (++) (mStation m) (mCelsius m : [])

run :: RIO App ()
run = do
  env <- ask
  logDebug "Running v0 (naive)..."
  content <- liftIO $ readFile $ aoInputFilePath $ view appOptionsL env
  let parsed = catMaybes $ map parser $ lines content
  logDebug
    =<< P.displayWithColor
      ( P.flow "First 10 parsed measurements:"
          <> P.line
          <> P.bulletedList (take 10 $ map (fromString . show) parsed)
      )
  let aggregated = execState (mapM_ addMeasurement parsed) Map.empty
  liftIO $ putStrLn $ buildFinalStr aggregated
