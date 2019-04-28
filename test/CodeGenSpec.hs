module CodeGenSpec where

import Test.Hspec
import AST
import CodeGen

spec :: Spec
spec = do
  describe "generateStatement" $ do
    context "with return" $ do
      it "should be \"ret\"" $ do
        generateStatement Return `shouldBe` "ret"

    context "with return 34" $ do
      it "should be \"    movl    $34, %eax \n    ret\"" $ do
        generateStatement (ReturnVal (Const (Int 34))) `shouldBe` "    movl    $34, %eax \n    ret"

    context "with return \'c\'" $ do
      it "should be \"    movl    $99, %eax \n    ret\"" $ do
        generateStatement (ReturnVal (Const (Char 'c'))) `shouldBe` "    movl    $99, %eax \n    ret"

    context "with return \'034\'" $ do
      it "should be \"    movl    $28, %eax \n    ret\"" $ do
        generateStatement (ReturnVal (Const (Oct "34"))) `shouldBe` "    movl    $28, %eax \n    ret"

    context "with return \'xFA34\'" $ do
      it "should be \"    movl    $64052, %eax \n    ret\"" $ do
        generateStatement (ReturnVal (Const (Hex "FA34"))) `shouldBe` "    movl    $64052, %eax \n    ret"

  describe "generateFunction" $ do
    context "with int main() { return 34; }" $ do
      it "should be \"main: \n    movl    $34, %eax \n    ret\"" $ do
        generateFunction (FuncDecl {typeDef = IntType, identifier = Id "main", funcParams = [], funcBody = FuncBody [ReturnVal (Const (Int 34))]}) `shouldBe` "main: \n    movl    $34, %eax \n    ret"

  describe "generate" $ do
    context "with int main() { return 34; }" $ do
      it "should be \"    .globl main\nmain: \n    movl    $34, %eax \n    ret\"" $ do
        generate (Program(FuncDecl {typeDef = IntType, identifier = Id "main", funcParams = [], funcBody = FuncBody [ReturnVal (Const (Int 34))]})) `shouldBe` "    .globl main\nmain: \n    movl    $34, %eax \n    ret"

main :: IO ()
main = hspec spec