module LexSpec (main, spec) where

import Test.Hspec
import Lex
import Text.ParserCombinators.Parsec hiding ((<|>), many)

spec :: Spec
spec = do
  describe "openParen" $ do
    it "should be Right OpenParen" $ do
      parse openParen "" "  ( " `shouldBe` Right OpenParen

  describe "closeParen" $ do
    it "should be Right CloseParen" $ do
      parse closeParen "" "  ) " `shouldBe` Right CloseParen

  describe "openBrace" $ do
    it "should be Right OpenBrace" $ do
      parse openBrace "" " {  " `shouldBe` Right OpenBrace

  describe "closeBrace" $ do
    it "should be Right CloseBrace" $ do
      parse closeBrace "" " }  " `shouldBe` Right CloseBrace

  describe "semiColon" $ do
    it "should be Right SemiColon" $ do
      parse semiColon "" "  ; " `shouldBe` Right SemiColon

  describe "negation" $ do
    it "should be Right Negation" $ do
      parse negation "" "  - " `shouldBe` Right Negation

  describe "bitComplement" $ do
    it "should be Right BitComplement" $ do
      parse bitComplement "" "  ~ " `shouldBe` Right BitComplement

  describe "logicalNegation" $ do
    it "should be Right LogicalNegation" $ do
      parse logicalNegation "" "  ! " `shouldBe` Right LogicalNegation

  describe "intKeyword" $ do
    it "should be Right IntKeyword" $ do
      parse intKeyword "" "  int " `shouldBe` Right IntKeyword

  describe "returnKeyword" $ do
    it "should be Right ReturnKeyword" $ do
      parse returnKeyword "" "  return " `shouldBe` Right ReturnKeyword

  describe "charKeyword" $ do
    it "should be Right CharKeyword" $ do
      parse charKeyword "" "  char " `shouldBe` Right CharKeyword

  describe "intLiteral" $ do
    it "should be Right (IntLiteral 457)" $ do
      parse intLiteral "" "  457 " `shouldBe` Right (IntLiteral 457)

  describe "identifier" $ do
    it "should be Right (Identifier main)" $ do
      parse identifier "" "  main " `shouldBe` Right (Identifier "main")

  describe "charLiteral" $ do
    context "without escape sequence" $ do
      it "should be Right (CharLiteral g)" $ do
        parse charLiteral "" "  'g' " `shouldBe` Right (CharLiteral 'g')

    context "with escape sequence" $ do
      it "should be Right (CharLiteral \b)" $ do
        parse charLiteral "" "  '\\b' " `shouldBe` Right (CharLiteral '\b')

  describe "octLiteral" $ do
    context "without quotes" $ do
      it "should be Right (OctLiteral 34)" $ do
        parse octLiteral "" "  034 " `shouldBe` Right (OctLiteral "34")

    context "with quotes" $ do
      it "should be Right (OctLiteral 34)" $ do
        parse octLiteral "" "  '\\034' " `shouldBe` Right (OctLiteral "34")

  describe "hexLiteral" $ do
    context "without quotes" $ do
      it "should be Right (HexLiteral FA34)" $ do
        parse hexLiteral "" "  0xFA34 " `shouldBe` Right (HexLiteral "FA34")

    context "with quotes" $ do
      it "should be Right (HexLiteral FA34)" $ do
        parse hexLiteral "" "  '\\xFA34' " `shouldBe` Right (HexLiteral "FA34")

  describe "lexer" $ do
    it "should be Right [IntKeyword, Identifier \"main\", OpenParen, CloseParen, OpenBrace, ReturnKeyword, IntLiteral 2, CharLiteral \'c\', CharLiteral \'\\b\', HexLiteral \"FA34\", , HexLiteral \"FA34\", OctLiteral \"34\", OctLiteral \"34\", SemiColon, CloseBrace]" $ do
      lexer "int main() {return 2 'c' '\\b' '\\xFA34' 0xFA34 '\\034' 034 ;} " `shouldBe` Right [IntKeyword, Identifier "main", OpenParen, CloseParen, OpenBrace, ReturnKeyword, IntLiteral 2, CharLiteral 'c', CharLiteral '\b', HexLiteral "FA34", HexLiteral "FA34", OctLiteral "34", OctLiteral "34", SemiColon, CloseBrace]


main :: IO ()
main = hspec spec