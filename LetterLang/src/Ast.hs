module Ast where

import Data.Text

data LetterAst
  = LetterProgram             LetterAst
  | LetterNumber              Integer
  | LetterString              Text
  | LetterBool                Bool
  | LetterNull
  | LetterStatements          [LetterAst]
  | LetterBlock               [LetterAst]
  | LetterEmptyStatement
  | LetterReturnStatement     (Maybe LetterAst)
  | LetterBinaryExpr          BinaryOp LetterAst LetterAst
  | LetterUnaryExpr           UnaryOp LetterAst
  | LetterMemberExpr          Bool LetterAst LetterAst
  | LetterCallExpr            LetterAst [LetterAst]
  | LetterThisExpr
  | LetterSuper
  | LetterNew                 LetterAst [LetterAst]
  | LetterAssign              LetterAst LetterAst
  | LetterIdentifier          Text
  | LetterLetDeclaration      [(LetterAst, Maybe LetterAst)]
  | LetterIfStatement         LetterAst LetterAst (Maybe LetterAst)
  | LetterWhileLoop           LetterAst LetterAst
  | LetterDoWhileLoop         LetterAst LetterAst
  | LetterForLoop             (Maybe LetterAst) (Maybe LetterAst) (Maybe LetterAst) LetterAst
  | LetterSequenceExpr        [LetterAst]
  | LetterFunctionDeclaration LetterAst [LetterAst] LetterAst
  | LetterClassDeclaration    LetterAst (Maybe LetterAst) LetterAst
  deriving (Show, Eq)

data BinaryOp
  = Plus | Minus | Multiply | Divide
  | Greater | GreaterEq | Less | LessEq
  | Equality | And | Or
  deriving (Show, Eq)

data UnaryOp
  = Positive | Negative | Negation
  deriving (Show, Eq)
