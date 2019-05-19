-- Data defnitions for the Abstrast Syntax Tree
module AST where

data Const =
        Int Int
      | Char Char
      | Oct String
      | Hex String
      | String String
      deriving (Show, Eq)

data UnaryOp = 
        Complement
      | Not
      | Negation
      | Plus
      deriving (Show, Eq, Ord)

data BinaryOp = 
        Add
      | Mult
      | Div
      | Sub
      | Mod
      deriving (Show, Eq, Ord)

data TypeDef =
        IntType
      | CharType
      deriving (Show, Eq)

data Expr = 
        ConstExpr Const 
      | UnOpExpr UnaryOp Expr
      | BinOpExpr BinaryOp Expr Expr
      deriving (Show, Eq)

data Statement = Return
               | ReturnVal Expr
               deriving (Show, Eq)

data Id = Id String deriving (Show, Eq)

data FuncParam = FuncParam TypeDef Id deriving (Show, Eq)

data FuncBody = FuncBody [Statement] deriving (Show, Eq)

data FuncDecl = FuncDecl{
    typeDef     :: TypeDef,
    identifier  :: Id,
    funcParams  :: [FuncParam],
    funcBody    :: FuncBody
} deriving (Eq, Show)

data Program = Program FuncDecl deriving (Eq, Show)