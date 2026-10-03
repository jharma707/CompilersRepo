module Ast where

import Data.Text

data EvaAst
  = EvaProgram        EvaAst
  | EvaNumber         Integer
  | EvaString         Text
  | EvaBool           Bool
  | EvaNull
  | EvaStatements     [EvaAst]
  | EvaBlock          [EvaAst]
  | EvaEmptyStatement
  | EvaBinaryExpr     BinaryOp EvaAst EvaAst
  | EvaUnaryExpr      UnaryOp EvaAst
  | EvaAssign         EvaAst EvaAst
  | EvaIdentifier     Text
  | EvaLetDeclaration [(EvaAst, Maybe EvaAst)]
  | EvaIfStatement    EvaAst EvaAst (Maybe EvaAst)
  | EvaWhileLoop      EvaAst EvaAst
  | EvaDoWhileLoop    EvaAst EvaAst
  | EvaForLoop        (Maybe EvaAst) (Maybe EvaAst) (Maybe EvaAst) EvaAst
  | EvaSequenceExpr   [EvaAst]
  deriving (Show, Eq)

data BinaryOp
  = Plus | Minus | Multiply | Divide
  | Greater | GreaterEq | Less | LessEq
  | Equality | And | Or
  deriving (Show, Eq)
data UnaryOp = Positive | Negative | Negation
  deriving (Show, Eq)
