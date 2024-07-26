module Parser
  ( Celsius (..),
    Observation (..),
    Station (..),
    unsafeParse,
  )
where

import RIO
import RIO.Text qualified as T

newtype Station = Station {unStation :: Text}
  deriving (Eq, Ord, Show)

newtype Celsius = Celsius {unCelsius :: Int16}
  deriving (Eq, Ord, Show)

data Observation = Observation
  { oStation :: {-# UNPACK #-} !Station,
    oCelsius :: {-# UNPACK #-} !Celsius
  }
  deriving (Eq, Ord, Show)

{- This will result in many false positives:
 - pass (bad) if the temperature has no fractional part e.g. a whole number
 - pass (bad) if the temperature has no integer part e.g. `.1`
 - pass (bad) if no station name is provided but there is still a ';'
-}
unsafeParse :: Text -> Maybe Observation
unsafeParse !line =
  case T.split (== ';') line of
    [!station, !celsiusStr] ->
      fmap
        (\(!val) -> Observation (Station station) (Celsius val))
        (readMaybe (T.unpack $ T.filter (/= '.') celsiusStr) :: Maybe Int16)
    _ -> Nothing
