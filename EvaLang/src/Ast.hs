module Ast where

import Data.Text

data EvaAst
  = EvaNumber         Integer
  | EvaString         Text
  | EvaBool           Bool
  | EvaStatements     [EvaAst]
  | EvaBlock          [EvaAst]
  | EvaEmptyStatement
  | EvaBinaryExpr     BinaryOp EvaAst EvaAst
  | EvaUnaryExpr      UnaryOp EvaAst
  | EvaAssign         Text EvaAst
  | EvaLetDeclaration [(Text, Maybe EvaAst)]
  | EvaIfStatement    EvaAst EvaAst (Maybe EvaAst)
  deriving (Show, Eq)

data BinaryOp
  = Plus | Minus | Multiply | Divide
  | Greater | GreaterEq | Less | LessEq
  | Equality | And | Or
  deriving (Show, Eq)
data UnaryOp  = Negative deriving (Show, Eq)
