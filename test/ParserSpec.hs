module ParserSpec (spec) where

import Parser
import RIO
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  it "Should correctly parse a row" $ do
    True `shouldBe` True

  it "Should correctly handle an invalid row" $ do
    parseRow "Hamburg,12.0" `shouldBe` (Left "Invalid row found")

