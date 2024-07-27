module Summary
  ( Summary,
    formatSummary,
    mergeSummary,
    mkInitialSummary,
  )
where

import Parser (Celsius (..))
import RIO
import Text.Printf

data Summary = Summary
  { sMin :: {-# UNPACK #-} !Int16,
    sMax :: {-# UNPACK #-} !Int16,
    sTotal :: {-# UNPACK #-} !Int64,
    sCount :: {-# UNPACK #-} !Int32
  }
  deriving (Show)

mkInitialSummary :: Celsius -> Summary
mkInitialSummary (Celsius !c) = Summary c c (fromIntegral c) 1

mergeSummary :: Summary -> Summary -> Summary
mergeSummary !s1 !s2 =
  let !sMin' = min (sMin s1) (sMin s2)
      !sMax' = max (sMax s1) (sMax s2)
      !sTotal' = sTotal s1 + sTotal s2
      !sCount' = sCount s1 + sCount s2
   in Summary sMin' sMax' sTotal' sCount'

formatSummary :: Summary -> ByteString
formatSummary !summary =
  let sMin' :: Double
      !sMin' = fromIntegral (sMin summary) / 10.0

      sMax' :: Double
      !sMax' = fromIntegral (sMax summary) / 10.0

      sMean' :: Double
      !sMean' = (fromIntegral (sTotal summary) / fromIntegral (sCount summary)) / 10.0
   in fromString $ printf "%.1f/%.1f/%.1f" sMin' sMean' sMax'
