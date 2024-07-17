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
  it "Should correctly handle postive Celsius temperatures" $ do
    parser "Hamburg;12.0\n" `shouldBe` (Just $ Measurement (Station "Hamburg") (Celsius 12.0))

  it "Should correctly handle negative Celsius temperatures" $ do
    parser "Hamburg;12.0\n" `shouldBe` (Just $ Measurement (Station "Hamburg") (Celsius 12.0))

  it "Should correctly handle an invalid row" $ do
    parser "Bogotá,12.0\n" `shouldBe` Nothing
    parser ";12.0\n" `shouldBe` Nothing
    parser "Tokyo;12\n" `shouldBe` Nothing


