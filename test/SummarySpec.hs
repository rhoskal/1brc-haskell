module SummarySpec (summarySpec) where

import RIO
import Summary
import Test.Hspec

summarySpec :: Spec
summarySpec = do
  describe "should handle negative numbers" $ do
    it "-0.00" $ do
      round' (-0.00 :: Double) `shouldBe` (0.0 :: Double)
    it "-0.01" $ do
      round' (-0.01 :: Double) `shouldBe` (0.0 :: Double)
    it "-0.02" $ do
      round' (-0.02 :: Double) `shouldBe` (0.0 :: Double)
    it "-0.03" $ do
      round' (-0.03 :: Double) `shouldBe` (0.0 :: Double)
    it "-0.04" $ do
      round' (-0.04 :: Double) `shouldBe` (0.0 :: Double)
    it "-0.05" $ do
      round' (-0.05 :: Double) `shouldBe` (0.0 :: Double)
    it "-0.06" $ do
      round' (-0.06 :: Double) `shouldBe` (-0.1 :: Double)
    it "-0.07" $ do
      round' (-0.07 :: Double) `shouldBe` (-0.1 :: Double)
    it "-0.08" $ do
      round' (-0.08 :: Double) `shouldBe` (-0.1 :: Double)
    it "-0.09" $ do
      round' (-0.09 :: Double) `shouldBe` (-0.1 :: Double)
    it "-0.10" $ do
      round' (-0.10 :: Double) `shouldBe` (-0.1 :: Double)
    it "-0.11" $ do
      round' (-0.11 :: Double) `shouldBe` (-0.1 :: Double)
    it "-0.12" $ do
      round' (-0.12 :: Double) `shouldBe` (-0.1 :: Double)
    it "-0.13" $ do
      round' (-0.13 :: Double) `shouldBe` (-0.1 :: Double)
    it "-0.14" $ do
      round' (-0.14 :: Double) `shouldBe` (-0.1 :: Double)
    it "-0.15" $ do
      round' (-0.15 :: Double) `shouldBe` (-0.1 :: Double)
    it "-0.16" $ do
      round' (-0.16 :: Double) `shouldBe` (-0.2 :: Double)
    it "-0.17" $ do
      round' (-0.17 :: Double) `shouldBe` (-0.2 :: Double)
    it "-0.18" $ do
      round' (-0.18 :: Double) `shouldBe` (-0.2 :: Double)
    it "-0.19" $ do
      round' (-0.19 :: Double) `shouldBe` (-0.2 :: Double)
    it "-0.20" $ do
      round' (-0.20 :: Double) `shouldBe` (-0.2 :: Double)
    it "-0.21" $ do
      round' (-0.21 :: Double) `shouldBe` (-0.2 :: Double)
    it "-0.22" $ do
      round' (-0.22 :: Double) `shouldBe` (-0.2 :: Double)
    it "-0.23" $ do
      round' (-0.23 :: Double) `shouldBe` (-0.2 :: Double)
    it "-0.24" $ do
      round' (-0.24 :: Double) `shouldBe` (-0.2 :: Double)
    it "-0.25" $ do
      round' (-0.25 :: Double) `shouldBe` (-0.2 :: Double)

  describe "should handle positive numbers" $ do
    it "0.00" $ do
      round' (0.00 :: Double) `shouldBe` (0.0 :: Double)
    it "0.01" $ do
      round' (0.01 :: Double) `shouldBe` (0.0 :: Double)
    it "0.02" $ do
      round' (0.02 :: Double) `shouldBe` (0.0 :: Double)
    it "0.03" $ do
      round' (0.03 :: Double) `shouldBe` (0.0 :: Double)
    it "0.04" $ do
      round' (0.04 :: Double) `shouldBe` (0.0 :: Double)
    it "0.05" $ do
      round' (0.05 :: Double) `shouldBe` (0.1 :: Double)
    it "0.06" $ do
      round' (0.06 :: Double) `shouldBe` (0.1 :: Double)
    it "0.07" $ do
      round' (0.07 :: Double) `shouldBe` (0.1 :: Double)
    it "0.08" $ do
      round' (0.08 :: Double) `shouldBe` (0.1 :: Double)
    it "0.09" $ do
      round' (0.09 :: Double) `shouldBe` (0.1 :: Double)
    it "0.10" $ do
      round' (0.10 :: Double) `shouldBe` (0.1 :: Double)
    it "0.11" $ do
      round' (0.11 :: Double) `shouldBe` (0.1 :: Double)
    it "0.12" $ do
      round' (0.12 :: Double) `shouldBe` (0.1 :: Double)
    it "0.13" $ do
      round' (0.13 :: Double) `shouldBe` (0.1 :: Double)
    it "0.14" $ do
      round' (0.14 :: Double) `shouldBe` (0.1 :: Double)
    it "0.15" $ do
      round' (0.15 :: Double) `shouldBe` (0.2 :: Double)
    it "0.16" $ do
      round' (0.16 :: Double) `shouldBe` (0.2 :: Double)
    it "0.17" $ do
      round' (0.17 :: Double) `shouldBe` (0.2 :: Double)
    it "0.18" $ do
      round' (0.18 :: Double) `shouldBe` (0.2 :: Double)
    it "0.19" $ do
      round' (0.19 :: Double) `shouldBe` (0.2 :: Double)
    it "0.20" $ do
      round' (0.20 :: Double) `shouldBe` (0.2 :: Double)
    it "0.21" $ do
      round' (0.21 :: Double) `shouldBe` (0.2 :: Double)
    it "0.22" $ do
      round' (0.22 :: Double) `shouldBe` (0.2 :: Double)
    it "0.23" $ do
      round' (0.23 :: Double) `shouldBe` (0.2 :: Double)
    it "0.24" $ do
      round' (0.24 :: Double) `shouldBe` (0.2 :: Double)
    it "0.25" $ do
      round' (0.25 :: Double) `shouldBe` (0.3 :: Double)

  describe "should handle others" $ do
    it "2.8499999999999996" $ do
      round' (2.8499999999999996 :: Double) `shouldBe` (2.8 :: Double)
    it "0.5499999999999998" $ do
      round' (0.5499999999999998 :: Double) `shouldBe` (0.5 :: Double)
    it "0.34999999999999987" $ do
      round' (0.34999999999999987 :: Double) `shouldBe` (0.3 :: Double)
    it "23.049999999999997" $ do
      round' (23.049999999999997 :: Double) `shouldBe` (23.0 :: Double)
    it "25.549999999999997" $ do
      round' (25.549999999999997 :: Double) `shouldBe` (25.5 :: Double)
    it "-1.0500000000000007" $ do
      round' (-1.0500000000000007 :: Double) `shouldBe` (-1.1 :: Double)
    it "11.649999999999999" $ do
      round' (11.649999999999999 :: Double) `shouldBe` (11.6 :: Double)
