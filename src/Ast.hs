module Ast where

data Stmt
  = Expr Expr
  | Print Expr
  | Var String (Maybe Expr)
  | Assign String Expr
  | Block [Stmt]
  | If Expr Stmt (Maybe Stmt)
  | While Expr Stmt
  | Function String [String] [Stmt]
  | Return (Maybe Expr)
  deriving (Show)

data Expr
  = Literal Literal
  | Unary UnaryOp Expr
  | Binary BinaryOp Expr Expr
  | Grouping Expr
  deriving (Show)

data Literal = Number' Double | String' String | Bool' Bool | Nil
  deriving (Show, Eq)

data UnaryOp = Minus' | Bang
  deriving (Show)

data BinaryOp
  = Slash
  | Star
  | Plus
  | Minus
  | And
  | Or
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | EqualEqual
  | BangEqual
  deriving (Show)
