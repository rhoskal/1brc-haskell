module Run (run) where

import Parser
import RIO
import RIO.ByteString qualified as B
import RIO.List (intersperse)
import RIO.Map qualified as Map
import RIO.PrettyPrint qualified as PP
import Summary
import Types

type Accumulator = Map Station Summary

strBuilder :: Accumulator -> ByteString
strBuilder !acc =
  let buildEntry :: (Station, Summary) -> ByteString
      buildEntry (Station !station, !summary) =
        station
          <> B.singleton 61 -- c2w '=' == 61
          <> formatSummary summary
   in B.singleton 123 -- c2w '{' == 123
        <> mconcat (intersperse (B.pack [44, 32]) (map buildEntry $ Map.toList acc))
        <> B.singleton 125 -- c2w '}' == 125
        <> B.singleton 10 -- c2w '\n' == 10

addObservation :: Accumulator -> Observation -> Accumulator
addObservation !acc !o =
  let !station = oStation o
      !summary = mkInitialSummary $ oCelsius o
   in Map.insertWith mergeSummary station summary acc

printResults :: Accumulator -> IO ()
printResults !acc = B.putStr $ strBuilder acc

lines' :: ByteString -> [ByteString]
lines' !ps
  | B.null ps = []
  | otherwise = case search ps of
      Nothing -> [ps]
      Just !n -> B.take n ps : lines' (B.drop (n + 1) ps)
  where
    search = B.elemIndex 10 -- c2w '\n' == 10

hUntilNewline :: Handle -> IO ByteString
hUntilNewline !fileHandle = go B.empty
  where
    go :: ByteString -> IO ByteString
    go !acc =
      B.hGet fileHandle 1 >>= \(!char) ->
        if char == "\n"
          then return acc
          else go $ B.append acc char

readChunk :: Handle -> IO ByteString
readChunk !fileHandle = do
  initialChunk <- B.hGet fileHandle 67108864
  isEOF <- hIsEOF fileHandle
  if isEOF
    then return initialChunk
    else B.append initialChunk <$> hUntilNewline fileHandle

parseFile :: FilePath -> IO Accumulator
parseFile !filePath = withFile filePath ReadMode $ \(!fileHandle) -> do
  let processChunk !acc = do
        isEOF <- hIsEOF fileHandle
        if isEOF
          then return acc
          else do
            chunk <- readChunk fileHandle
            let !observations = map unsafeParse $ lines' chunk
            let !newAcc = foldl' addObservation acc observations
            processChunk newAcc
  processChunk Map.empty

run :: RIO App ()
run = do
  env <- ask
  logDebug "Running v6..."
  processed <- liftIO $ parseFile (aoInputFilePath $ view appOptionsL env)
  logDebug
    =<< PP.displayWithColor
      ( PP.flow "First 10 processed:"
          <> PP.line
          <> PP.bulletedList (take 10 $ map (fromString . show) $ Map.toList processed)
      )
  liftIO $ printResults processed
