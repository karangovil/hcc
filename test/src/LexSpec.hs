module LexSpec (main, spec) where

import Test.Hspec
import Lex
import Text.ParserCombinators.Parsec hiding ((<|>), many)

spec :: Spec
spec = do
  describe "openParen" $ do
    it "parses open parens as a Token" $ do
      parse openParen "" "(" `shouldBe` Token.OpenParen

main :: IO ()
main = hspec spec