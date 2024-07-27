import Criterion.Main
import Parser
import RIO
import Summary

main :: IO ()
main =
  defaultMain
    [ bgroup
        "Parser"
        [ bench "unsafeParse" $ whnf unsafeParse "Belgium;12.9"
        ],
      bgroup
        "Summary"
        [ bench "mergeSummary" $ whnf (uncurry mergeSummary) (mkInitialSummary (Celsius 490), mkInitialSummary (Celsius 180)),
          bench "formatSummary" $ whnf formatSummary $ mkInitialSummary (Celsius 990)
        ]
    ]
