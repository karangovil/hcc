module LexSpec (main, spec) where

import Test.Hspec
import Lex
import Text.ParserCombinators.Parsec hiding ((<|>), many)

spec :: Spec
spec = do
  describe "openParen" $ do
    context "with (" $ do
      it "should be Right OpenParen" $ do
        parse openParen "" "(" `shouldBe` Right OpenParen

    context "with (spaces) (" $ do
      it "should be Right OpenParen" $ do
        parse openParen "" "  ( " `shouldBe` Right OpenParen

main :: IO ()
main = hspec spec