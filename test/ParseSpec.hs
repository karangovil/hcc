module ParseSpec (main, spec) where

import Test.Hspec
import Lex hiding (identifier)
import AST
import Parse

spec :: Spec
spec = do
  describe "parseFunctionParams" $ do
    context "with )" $ do
      it "should be ([],[])" $ do
        parseFunctionParams [CloseParen] `shouldBe` ([],[])

    context "with int x)" $ do
      it "should be ([FuncParam IntType (Id \"x\")],[])" $ do
        parseFunctionParams [IntKeyword, Identifier "x", CloseParen] `shouldBe` ([FuncParam IntType (Id "x")],[])

    context "with char x)" $ do
      it "should be ([FuncParam CharType (Id \"x\")],[])" $ do
        parseFunctionParams [CharKeyword, Identifier "x", CloseParen] `shouldBe` ([FuncParam CharType (Id "x")],[])

  describe "parseExpr" $ do
    context "with 34" $ do
      it "should be (Const (Int 34),[])" $ do
        parseExpr [IntLiteral 34] `shouldBe` (Const (Int 34),[])

    context "with \'c\'" $ do
      it "should be (Const (Char \'c\'),[])" $ do
        parseExpr [CharLiteral 'c'] `shouldBe` (Const (Char 'c'),[])

    context "with 034" $ do
      it "should be (Const (Oct \"34\"),[])" $ do
        parseExpr [OctLiteral "34"] `shouldBe` (Const (Oct "34"),[])

    context "with 0xFA34" $ do
      it "should be (Const (Hex \"FA34\"),[])" $ do
        parseExpr [HexLiteral "FA34"] `shouldBe` (Const (Hex "FA34"),[])

  describe "parseStatementList" $ do
    context "with return;" $ do
      it "should be ([Return],[])" $ do
        parseStatementList [ReturnKeyword, SemiColon] `shouldBe` ([Return],[])

    context "with return 34;" $ do
      it "should be ([ReturnVal (Const (Int 34))],[])" $ do
        parseStatementList [ReturnKeyword, IntLiteral 34, SemiColon] `shouldBe` (([ReturnVal (Const (Int 34))],[]))

    context "with int x" $ do
      it "should be ([],[IntKeyword, Identifier \"x\"])" $ do
        parseStatementList [IntKeyword, Identifier "x"] `shouldBe` ([], [IntKeyword, Identifier "x"])

  describe "parseFunctionBody" $ do
    context "with return 34;" $ do
      it "should be (FuncBody [ReturnVal (Const (Int 34))])" $ do
        parseFunctionBody [ReturnKeyword, IntLiteral 34, SemiColon, CloseBrace] `shouldBe` (FuncBody [ReturnVal (Const (Int 34))])

    context "with return;" $ do
      it "should be (FuncBody [Return])" $ do
        parseFunctionBody [ReturnKeyword, SemiColon, CloseBrace] `shouldBe` (FuncBody [Return])

  describe "parseFunction" $ do
    context "with int main() { return; }" $ do
      it "should be (FuncDecl {typeDef = IntType, identifier = Id \"main\", funcParams = [], funcBody = FuncBody [Return]})" $ do
        parseFunction [IntKeyword, Identifier "main", OpenParen, CloseParen, OpenBrace,ReturnKeyword, SemiColon, CloseBrace] `shouldBe` (FuncDecl {typeDef = IntType, identifier = Id "main", funcParams = [], funcBody = FuncBody [Return]})

    context "with int main(int x) { return; }" $ do
      it "should be (FuncDecl {typeDef = IntType, identifier = Id \"main\", funcParams = [FuncParam IntType (Id \"x\")], funcBody = FuncBody [Return]})" $ do
        parseFunction [IntKeyword, Identifier "main", OpenParen, IntKeyword, Identifier "x", CloseParen, OpenBrace, ReturnKeyword, SemiColon, CloseBrace] `shouldBe` (FuncDecl {typeDef = IntType, identifier = Id "main", funcParams = [FuncParam IntType (Id "x")], funcBody = FuncBody [Return]})

    context "with int main() { return 34; }" $ do
      it "should be (FuncDecl {typeDef = IntType, identifier = Id \"main\", funcParams = [FuncParam IntType (Id \"x\")], funcBody = FuncBody [ReturnVal (Const (Int 34))]})" $ do
        parseFunction [IntKeyword, Identifier "main", OpenParen, IntKeyword, Identifier "x", CloseParen, OpenBrace, ReturnKeyword, IntLiteral 34, SemiColon, CloseBrace] `shouldBe` (FuncDecl {typeDef = IntType, identifier = Id "main", funcParams = [FuncParam IntType (Id "x")], funcBody = FuncBody [ReturnVal (Const (Int 34))]})

  describe "parseProgram" $ do
    it "should be (Program(FuncDecl {typeDef = IntType, identifier = Id \"main\", funcParams = [FuncParam IntType (Id \"x\")], funcBody = FuncBody [ReturnVal (Const (Int 34))]}))" $ do
        parseProgram [IntKeyword, Identifier "main", OpenParen, IntKeyword, Identifier "x", CloseParen, OpenBrace, ReturnKeyword, IntLiteral 34, SemiColon, CloseBrace] `shouldBe` (Program(FuncDecl {typeDef = IntType, identifier = Id "main", funcParams = [FuncParam IntType (Id "x")], funcBody = FuncBody [ReturnVal (Const (Int 34))]}))
        

main :: IO ()
main = hspec spec