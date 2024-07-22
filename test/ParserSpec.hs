module ParserSpec (parserSpec) where

import Parser
  ( Celsius (..),
    Measurement (..),
    Station (..),
    parser,
  )
import RIO
import RIO.Text qualified as T
import Test.Hspec
import Test.QuickCheck

parserSpec :: Spec
parserSpec = do
  describe "parser" $ do
    it "parses all possible Celsius values (neg & pos) correctly"
      $ property
      $ \(intPart :: Int) (fracPart :: Int) ->
        let stationName :: Text
            stationName = "TestStation"

            celsiusText :: Text
            celsiusText =
              (T.pack $ show intPart)
                <> (T.singleton '.')
                <> (T.pack $ show $ abs fracPart)

            input :: Text
            input = stationName <> T.singleton ';' <> celsiusText
         in case parser input of
              Just (Measurement (Station s) (Celsius t)) ->
                let maybeCelsius = readMaybe $ T.unpack celsiusText :: Maybe Float
                 in s == stationName && (Just t) == maybeCelsius
              _ -> False
    it "Should correctly handle an invalid row" $ do
      parser "Bogotá,12.0" `shouldBe` Nothing
      parser ";12.0" `shouldBe` Nothing
      parser "Tokyo;12" `shouldBe` Nothing
