module Parser
  ( Celsius (..),
    Observation (..),
    Station (..),
    unsafeParse,
  )
where

import RIO
import RIO.ByteString qualified as B

newtype Station = Station {unStation :: ByteString}
  deriving (Eq, Ord, Show)

newtype Celsius = Celsius {unCelsius :: Double}
  deriving (Eq, Ord, Show)

data Observation = Observation
  { oStation :: {-# UNPACK #-} !Station,
    oCelsius :: {-# UNPACK #-} !Celsius
  }
  deriving (Eq, Ord, Show)

pCelsius :: ByteString -> Double
pCelsius !input =
  -- c2w '-' == 45
  case B.index input 0 of
    45 -> negate $ pCelsius (B.drop 1 input)
    !c1 ->
      -- c2w '.' == 46
      case B.index input 1 of
        46 ->
          let !c2 = B.index input 2
           in fromIntegral (d2i c1 * 10 + d2i c2) / 10.0
        !c2 ->
          let !c3 = B.index input 3
           in fromIntegral (d2i c1 * 100 + d2i c2 * 10 + d2i c3) / 10.0
  where
    -- c2w '0' == 48
    d2i :: Word8 -> Int
    d2i !c = fromIntegral c - 48

unsafeParse :: ByteString -> Observation
unsafeParse !input =
  -- c2w ';' == 59
  case B.findIndex (== 59) input of
    Nothing -> error $ "no ';' found in " <> show input
    Just !n ->
      let station :: Station
          !station = Station $ B.take n input

          celsius :: Celsius
          !celsius = Celsius $ pCelsius $ B.drop (n + 1) input
       in Observation station celsius
