module Parser where

import RIO

newtype Station = Station Text
  deriving (Eq, Show)

newtype Measurement = Measurement Float
  deriving (Eq, Show)

data Row = Row Station Measurement
  deriving (Eq, Show)

parseRow :: Text -> Either Text Row
parseRow = undefined
