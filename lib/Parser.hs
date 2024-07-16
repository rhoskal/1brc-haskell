module Parser where

import RIO

data Row = Row Text Text deriving (Eq, Show)

parseRow :: Text -> Row
parseRow = undefined
