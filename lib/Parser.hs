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
import RIO.Text qualified as T

newtype Parser a = Parser {runParser :: Text -> Maybe (Text, a)}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap fn (Parser p) = Parser $ \input -> do
    (rest, matched) <- p input
    return (rest, fn matched)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \input -> Just (input, x)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (rest, fn) <- p1 input
    (rest', matched) <- p2 rest
    return (rest', fn matched)

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser p1) <|> (Parser p2) = Parser $ \input ->
    p1 input <|> p2 input

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (Parser p1) >>= fn = Parser $ \input -> do
    (rest, matched) <- p1 input
    let Parser p2 = fn matched
    p2 rest

  return :: a -> Parser a
  return = pure

-- Combinators

charP :: Char -> Parser Char
charP input = Parser fn
  where
    fn txt
      | T.null txt = Nothing
      | otherwise =
          T.uncons txt
            >>= \(y, ys) ->
              if y == input
                then Just (ys, input)
                else Nothing

many1 :: (Char -> Bool) -> Parser Text
many1 predicate = Parser $ \input ->
  let (matched, rest) = T.span predicate input
   in if T.null matched
        then Nothing
        else Just (rest, matched)

optionalP :: Parser a -> Parser (Maybe a)
optionalP (Parser p) = Parser $ \input ->
  case p input of
    Nothing -> Just (input, Nothing)
    Just (rest, matched) -> Just (rest, Just matched)

digitsP :: Parser Text
digitsP = many1 C.isDigit

-- Parsers

newtype Station = Station {unStation :: Text}
  deriving (Eq, Ord, Show)

newtype Celsius = Celsius {unCelsius :: Float}
  deriving (Eq, Num, Ord, Show)

instance Fractional Celsius where
  (/) :: Celsius -> Celsius -> Celsius
  (Celsius c1) / (Celsius c2) = Celsius $ c1 / c2

  fromRational :: Rational -> Celsius
  fromRational = realToFrac

data Measurement = Measurement
  { mStation :: !Station,
    mCelsius :: !Celsius
  }
  deriving (Eq, Ord, Show)

pStation :: Parser Station
pStation = Station <$> many1 (/= ';')

pCelsius :: Parser Celsius
pCelsius = do
  maybeSign <- optionalP $ charP '-'
  intPart <- digitsP
  _ <- charP '.'
  fracPart <- digitsP
  let sign :: Text
      sign = maybe T.empty (const $ T.pack "-") maybeSign

      numStr :: Text
      !numStr = sign <> intPart <> T.pack "." <> fracPart
  case readMaybe (T.unpack numStr) of
    Just !floatValue -> return $ Celsius floatValue
    Nothing -> empty

pMeasurement :: Parser Measurement
pMeasurement = Measurement <$> pStation <*> (charP ';' *> pCelsius)

parser :: Text -> Maybe Measurement
parser input = snd <$> runParser pMeasurement input
