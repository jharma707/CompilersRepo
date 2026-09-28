module Ast where

import Data.Text

data EvaAst
  = EvaNumber         Integer
  | EvaString         Text
  | EvaStatements     [EvaAst]
  | EvaBlock          [EvaAst]
  | EvaEmptyStatement
  | EvaBinaryExpr     BinaryOp EvaAst EvaAst
  | EvaUnaryExpr      UnaryOp EvaAst
  deriving (Show, Eq)

data BinaryOp = Plus | Minus | Multiply | Divide deriving (Show, Eq)
data UnaryOp  = Negative deriving (Show, Eq)
