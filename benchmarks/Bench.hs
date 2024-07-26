import Criterion.Main
import Parser
import RIO
import RIO.Text qualified as T
import Summary

parseLines :: [Text] -> [Maybe Observation]
parseLines = map unsafeParse

main :: IO ()
main =
  defaultMain
    [ bgroup
        "Parser"
        [ bench "unsafeParse 10_000_000" $ whnf parseLines $ gen 10000000
        ],
      bgroup
        "Summary"
        [ bench "mergeSummary" $ whnf (uncurry mergeSummary) (mkInitialSummary (Celsius 490), mkInitialSummary (Celsius 180)),
          bench "formatSummary" $ whnf formatSummary $ mkInitialSummary (Celsius 990)
        ]
    ]
  where
    gen :: Int -> [Text]
    gen = T.lines . flip T.replicate "Belgium;12.9\n"
