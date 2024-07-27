module ParserSpec (parserSpec) where

import Parser
  ( Celsius (..),
    Observation (..),
    Station (..),
    unsafeParse,
  )
import RIO
import RIO.ByteString qualified as B
import Test.Hspec
import Test.QuickCheck

genCelsius :: Gen (ByteString, ByteString)
genCelsius = do
  intPart <- choose (-99, 99) :: Gen Int
  fracPart <- choose (0, 9) :: Gen Int
  let i = fromString $ show intPart
      f = fromString $ show fracPart
   in return (i, f)

parserSpec :: Spec
parserSpec = do
  describe "parser" $ do
    it "Should parse all possible Celsius values (-99.9 to 99.9) correctly"
      $ property
      $ forAll genCelsius
      $ \(intPart, fracPart) ->
        let stationName :: ByteString
            stationName = "TestStation"

            celsiusBS :: ByteString
            celsiusBS =
              intPart
                <> B.singleton 46 -- c2w '.' == 46
                <> fracPart

            input :: ByteString
            input =
              stationName
                <> B.singleton 59 -- c2w ';' == 59
                <> celsiusBS

            (Observation (Station s) (Celsius c)) = unsafeParse input
         in (s == stationName) && fromString (show c) == B.filter (/= 46) celsiusBS

    it "Should correctly handle edge cases" $ do
      evaluate (unsafeParse "Buenos Aires,12.0") `shouldThrow` errorCall "no ';' found in \"Buenos Aires,12.0\""
      evaluate (unsafeParse "Tokyo;12") `shouldThrow` anyErrorCall
      evaluate (unsafeParse "Aukland;.2") `shouldThrow` anyErrorCall
      unsafeParse ";12.0" `shouldBe` Observation (Station "") (Celsius 120)
