module Summary where

import Parser (Celsius (..))
import RIO
import RIO.ByteString qualified as B

data Summary = Summary
  { sMin :: {-# UNPACK #-} !Double,
    sMax :: {-# UNPACK #-} !Double,
    sTotal :: {-# UNPACK #-} !Double,
    sCount :: {-# UNPACK #-} !Int32
  }
  deriving (Show)

mkInitialSummary :: Celsius -> Summary
mkInitialSummary (Celsius !c) = Summary c c c 1

mergeSummary :: Summary -> Summary -> Summary
mergeSummary !s1 !s2 =
  let !sMin' = min (sMin s1) (sMin s2)
      !sMax' = max (sMax s1) (sMax s2)
      !sTotal' = sTotal s1 + sTotal s2
      !sCount' = sCount s1 + sCount s2
   in Summary sMin' sMax' sTotal' sCount'

-- | Mimic Java's `Math.round()`
round' :: Double -> Double
round' n =
  let scaled :: Double
      scaled = n * 10.0

      r :: Double
      r = fromIntegral (round scaled :: Int)

      rounded :: Double
      rounded
        | abs (r - scaled) == 0.5 =
            if n < 0.0
              then signum n * fromIntegral (floor (abs scaled) :: Int)
              else signum n * fromIntegral (ceiling (abs scaled) :: Int)
        | otherwise = fromIntegral (round scaled :: Int)
   in rounded / 10.0

formatSummary :: Summary -> ByteString
formatSummary (Summary !sMin' !sMax' !sTotal' !sCount') =
  B.concat
    [ fromString $ show $ round' sMin',
      "/",
      fromString $ show $ (sTotal' / fromIntegral sCount'),
      "/",
      fromString $ show $ round' sMax'
    ]


