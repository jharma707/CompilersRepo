module Ast (EvaAst(..)) where

data EvaAst
  = EvaNumber     Integer
  | EvaString     String
  | EvaStatements [EvaAst]
  deriving (Show, Eq)
