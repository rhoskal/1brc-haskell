module ParserSpec (parserSpec) where

import Parser
  ( Celsius (..),
    Measurement (..),
    Station (..),
    parser,
  )
import RIO
import RIO.Partial (read)
import Test.Hspec
import Test.QuickCheck

parserSpec :: Spec
parserSpec = do
  describe "parser" $ do
    it "parses all possible Celsius values (neg & pos) correctly"
      $ property
      $ \x y ->
        let stationName = "TestStation"
            celsiusStr = show (x :: Int) <> "." <> show (abs y :: Int)
            input = stationName ++ ";" ++ celsiusStr
         in case parser input of
              Just (Measurement (Station s) (Celsius t)) ->
                s == stationName && t == (read celsiusStr :: Float)
              _ -> False
    it "Should correctly handle an invalid row" $ do
      parser "Bogotá,12.0" `shouldBe` Nothing
      parser ";12.0" `shouldBe` Nothing
      parser "Tokyo;12" `shouldBe` Nothing
