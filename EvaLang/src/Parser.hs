module Parser (parseEva) where

import Ast

import Control.Arrow
import qualified Data.Text as T
import Data.Function
import Text.Parsec
import Text.Parsec.Text (Parser)

parseEva :: T.Text -> Either ParseError EvaAst
parseEva = parse evaProgram ""

evaProgram :: Parser EvaAst
evaProgram = evaStatementList

evaStatementList :: Parser EvaAst
evaStatementList = EvaStatements <$> many1 evaStatement

evaBlock :: Parser EvaAst
evaBlock = EvaBlock <$> (between start end (many evaStatement)) where
  start = char '{' >> spaces
  end   = char '}' >> spaces

evaEmptyStatement :: Parser EvaAst
evaEmptyStatement = const EvaEmptyStatement <$> (char ';' >> spaces)

evaStatement :: Parser EvaAst
evaStatement = choice
  [ evaExpressionStatement
  , evaBlock
  , evaEmptyStatement
  ]

evaExpressionStatement :: Parser EvaAst
evaExpressionStatement = evaExpression <* spaces <* char ';' <* spaces

evaExpression :: Parser EvaAst
evaExpression = evaBinaryExpression

evaBinaryExpression :: Parser EvaAst
evaBinaryExpression = addExpr where
  expr :: String -> Parser EvaAst -> Parser EvaAst
  expr ops subexpr = do
    leftExpr  <- subexpr
    restExprs <- many $ (\op a b -> EvaBinaryExpr op b a) <$> binaryOp ops <*> subexpr
    return $ foldl (&) leftExpr restExprs

  binaryOp ops = (try (spaces *> choice (char <$> ops) <* spaces)) >>= toOp

  addExpr = expr "+-" multExpr
  multExpr = expr "*/" primaryExpr
  primaryExpr = choice [evaLiteral, parenExpr]
  parenExpr = char '(' *> spaces *> evaExpression <* spaces <* char ')'

evaLiteral :: Parser EvaAst
evaLiteral = choice [evaNumber, evaString]

evaNumber :: Parser EvaAst
evaNumber = (read >>> EvaNumber) <$> many1 digit

evaString :: Parser EvaAst
evaString = (T.pack >>> EvaString) <$> between (char '"') (char '"') (many $ noneOf "\"")

-- helper functions
toOp :: Char -> Parser BinaryOp
toOp '+' = return Plus
toOp '-' = return Minus
toOp '*' = return Multiply
toOp '/' = return Divide
toOp op  = parserFail $ "unexpected binary expression: " <> [op]
