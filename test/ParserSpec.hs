module ParserSpec (spec) where

import Parser
  ( Celsius (..),
    Measurement (..),
    Station (..),
    parser,
  )
import RIO
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  it "Should correctly parse a row" $ do
    parser "Hamburg;12.0\n" `shouldBe` (Just $ Measurement (Station "Hamburg") (Celsius 12.0))

  it "Should correctly handle an invalid row" $ do
    parser "Hamburg,12.0\n" `shouldBe` Nothing


