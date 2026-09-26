module Ast (EvaAst(..)) where

import Data.Text

data EvaAst
  = EvaNumber         Integer
  | EvaString         Text
  | EvaStatements     [EvaAst]
  | EvaBlock          [EvaAst]
  | EvaEmptyStatement
  deriving (Show, Eq)
