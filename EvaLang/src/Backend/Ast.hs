module Backend.Ast where

import Data.Text

data EvaAst
  = EvaNumber         Integer
  | EvaString         Text
  | EvaStatements     [EvaAst]
  | EvaBlock          [EvaAst]
  | EvaEmptyStatement
  | EvaBinaryExpr     BinaryOp EvaAst EvaAst
  deriving (Show, Eq)

data BinaryOp = Plus | Minus | Multiply | Divide deriving (Show, Eq)
