module Statistics
  ( Statistics,
    mkStatistics,
    toString,
  )
where

import Parser (Celsius (..))
import RIO
import RIO.List qualified as List
import RIO.Text qualified as T

data Statistics = Statistics
  { sMin :: !Float,
    sMean :: !Float,
    sMax :: !Float
  }

toString :: Statistics -> Text
toString stat =
  (T.pack $ show $ sMin stat)
    <> T.singleton '/'
    <> (T.pack $ show $ roundTowardPositive $ sMean stat)
    <> T.singleton '/'
    <> (T.pack $ show $ sMax stat)

roundTowardPositive :: (RealFrac a) => a -> Float
roundTowardPositive n = fromIntegral ((ceiling $ n * 10) :: Integer) / 10.0

meanMaybe :: (Fractional a) => [a] -> Maybe a
meanMaybe xs
  | null xs = Nothing
  | otherwise = Just $ List.sum xs / (List.genericLength xs)

min' :: [Celsius] -> Float
min' [c] = unCelsius c
min' cs = maybe 0.0 unCelsius $ List.minimumMaybe cs

arithmeticMean :: [Celsius] -> Float
arithmeticMean [c] = unCelsius c
arithmeticMean cs = maybe 0.0 unCelsius $ meanMaybe cs

max' :: [Celsius] -> Float
max' [c] = unCelsius c
max' cs = maybe 0.0 unCelsius $ List.maximumMaybe cs

mkStatistics :: [Celsius] -> Statistics
mkStatistics cs = Statistics (min' cs) (arithmeticMean cs) (max' cs)
