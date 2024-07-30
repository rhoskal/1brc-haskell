import ParserSpec (parserSpec)
import RIO
import SummarySpec (summarySpec)
import Test.Hspec

main :: IO ()
main = hspec $ parallel $ do
  describe "Parser" parserSpec
  describe "Rounding" summarySpec
