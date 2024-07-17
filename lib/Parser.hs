module Parser
  ( Celsius (..),
    Measurement (..),
    Station (..),
    parser,
  )
where

import Control.Applicative (empty)
import RIO
import RIO.Char qualified as C

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap fn (Parser p) = Parser $ \input -> do
    (rest, matched) <- p input
    return (rest, fn matched)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \input -> Just (input, x)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) (Parser p1) (Parser p2) = Parser $ \input -> do
    (input', fn) <- p1 input
    (input'', x) <- p2 input'
    return (input'', fn x)

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser p1) (Parser p2) = Parser $ \input ->
    p1 input <|> p2 input

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (Parser p1) >>= f = Parser $ \input -> do
    (rest, matched) <- p1 input
    let Parser p2 = f matched
     in p2 rest

  return :: a -> Parser a
  return = pure

-- Combinators

charP :: Char -> Parser Char
charP input = Parser f
  where
    f [] = Nothing
    f (y : ys)
      | y == input = Just (ys, input)
      | otherwise = Nothing

stringP :: String -> Parser String
stringP = sequenceA . map charP

many1 :: (Char -> Bool) -> Parser String
many1 predicate = Parser $ \input ->
  let (matched, rest) = span predicate input
   in if (length matched == 0)
        then Nothing
        else Just (rest, matched)

digitsP :: Parser String
digitsP = many1 C.isDigit

eol :: Parser String
eol =
  stringP "\n\r"
    <|> stringP "\r\n"
    <|> stringP "\n"
    <|> stringP "\r"

-- Parsers

newtype Station = Station String
  deriving (Eq, Show)

newtype Celsius = Celsius Float
  deriving (Eq, Show)

data Measurement = Measurement Station Celsius
  deriving (Eq, Show)

pStation :: Parser Station
pStation = Station <$> many1 ((/=) ';')

pCelsius :: Parser Celsius
pCelsius =
  Celsius
    <$> ( digitsP >>= \intPart ->
            charP '.' >>= \_ ->
              digitsP >>= \fracPart ->
                case readMaybe (intPart ++ "." ++ fracPart) of
                  Just floatValue -> return floatValue
                  Nothing -> empty
        )

pMeasurement :: Parser Measurement
pMeasurement = Measurement <$> pStation <*> (charP ';' *> pCelsius <* eol)

parser :: String -> Maybe Measurement
parser input = snd <$> runParser pMeasurement input
