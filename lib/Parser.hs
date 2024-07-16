module Parser where

import RIO

data Row = Row
  { rowCity :: !Text,
    rowTemperature :: !Float
  }
  deriving (Eq, Show)

parse :: ()
parse = undefined
