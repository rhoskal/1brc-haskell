module ParserSpec (parserSpec) where

import Parser
  ( Celsius (..),
    Observation (..),
    Station (..),
    unsafeParse,
  )
import RIO
import RIO.Text qualified as T
import Test.Hspec
import Test.QuickCheck

genFracPart :: Gen Int
genFracPart = choose (0, 9)

parserSpec :: Spec
parserSpec = do
  describe "parser" $ do
    it "Should parse all possible Celsius values (neg & pos) correctly"
      $ property
      $ \(intPart :: Int) -> forAll genFracPart $ \fracPart ->
        let stationName :: Text
            stationName = "TestStation"

            celsiusText :: Text
            celsiusText =
              (T.pack $ show intPart)
                <> (T.singleton '.')
                <> (T.pack $ show fracPart)

            input :: Text
            input = stationName <> T.singleton ';' <> celsiusText
         in case unsafeParse input of
              Just (Observation (Station s) (Celsius c)) ->
                let maybeCelsius :: Maybe Int16
                    maybeCelsius = readMaybe $ T.unpack $ T.filter (/= '.') celsiusText
                 in s == stationName && (Just c) == maybeCelsius
              _ -> False
    it "Should correctly handle an invalid row" $ do
      unsafeParse "Bogotá,12.0" `shouldBe` Nothing
      unsafeParse "Bogotá;" `shouldBe` Nothing
      (isJust $ unsafeParse ";12.0") `shouldBe` True -- false positive
      (isJust $ unsafeParse "Tokyo;12") `shouldBe` True -- false positive
      (isJust $ unsafeParse "Aukland;.2") `shouldBe` True -- false positive
