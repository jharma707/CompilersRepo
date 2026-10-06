-- A very basic tree walk interpreter for testing purposes.
module Backend.TreeWalkInterpreter where

import Ast
import qualified Data.Text as T
import qualified GHC.List as L

data LetterValue
  = VInt    Integer
  | VString T.Text
  | VNone
  deriving (Eq)

instance Show LetterValue where
  show (VInt i) = show i
  show (VString str) = show str
  show VNone = ""

-- TODO: add environment
-- TODO: return an Either here for error reporting
interpretLetterAst :: LetterAst -> LetterValue
interpretLetterAst (LetterNumber i) = VInt i
interpretLetterAst (LetterString str) = VString str
interpretLetterAst (LetterStatements stmts) = last $ interpretLetterAst <$> stmts
interpretLetterAst (LetterBlock block) =
  case (L.unsnoc (interpretLetterAst <$> block)) of
    Nothing     -> VNone
    Just (_, x) -> x
interpretLetterAst LetterEmptyStatement = VNone
interpretLetterAst (LetterBinaryExpr op a b) =
  let v1 = interpretLetterAst a
      v2 = interpretLetterAst b
  in execBinaryOp op v1 v2 where

  -- no type checking, yet
  execBinaryOp Plus (VInt i) (VInt j) = VInt $ i + j
  execBinaryOp Minus (VInt i) (VInt j) = VInt $ i - j
  execBinaryOp Multiply (VInt i) (VInt j) = VInt $ i * j
  execBinaryOp Divide (VInt i) (VInt j) = VInt $ i `div` j -- catch divide by zero errors
  execBinaryOp Plus (VString str1) (VString str2) = VString $ str1 `T.append` str2
  execBinaryOp _ _ _ = error "type error: binary operator on invalid expression"
interpretLetterAst (LetterUnaryExpr Negative a) =
  case interpretLetterAst a of
    (VInt i) -> VInt $ -i
    _        -> error "type error: unary operator on invalid expression"
-- for now. TODO: save the value into the environment
interpretLetterAst (LetterAssign _ v) = interpretLetterAst v
