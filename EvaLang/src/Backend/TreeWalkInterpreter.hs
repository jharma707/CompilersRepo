-- A very basic tree walk interpreter for testing purposes.
module Backend.TreeWalkInterpreter where

import Backend.Ast
import qualified Data.Text as T
import qualified GHC.List as L

data EvaValue
  = VInt    Integer
  | VString T.Text
  | VNone
  deriving (Eq)

instance Show EvaValue where
  show (VInt i) = show i
  show (VString str) = show str
  show VNone = ""

-- TODO: add environment
-- TODO: return an Either here for error reporting
interpretEvaAst :: EvaAst -> EvaValue
interpretEvaAst (EvaNumber i) = VInt i
interpretEvaAst (EvaString str) = VString str
interpretEvaAst (EvaStatements stmts) = last $ interpretEvaAst <$> stmts
interpretEvaAst (EvaBlock block) =
  case (L.unsnoc (interpretEvaAst <$> block)) of
    Nothing     -> VNone
    Just (_, x) -> x
interpretEvaAst EvaEmptyStatement = VNone
interpretEvaAst (EvaBinaryExpr op a b) =
  let v1 = interpretEvaAst a
      v2 = interpretEvaAst b
  in execBinaryOp op v1 v2 where

  execBinaryOp Plus (VInt i) (VInt j) = VInt $ i + j
  execBinaryOp Minus (VInt i) (VInt j) = VInt $ i - j
  execBinaryOp Multiply (VInt i) (VInt j) = VInt $ i * j
  execBinaryOp Divide (VInt i) (VInt j) = VInt $ i `div` j -- catch divide by zero errors
  execBinaryOp Plus (VString str1) (VString str2) = VString $ str1 `T.append` str2
  execBinaryOp op v1 v2 = error $ "type error: cannot perform '" <> show op <> "' on " <> show v1 <> " and " <> show v2
