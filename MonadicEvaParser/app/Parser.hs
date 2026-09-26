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
  addExpr :: Parser EvaAst
  addExpr = do
    leftExpr  <- multExpr
    restExprs <- manyExprs ['+', '-'] multExpr
    return $ combineExprs leftExpr restExprs

  multExpr :: Parser EvaAst
  multExpr = do
    leftExpr  <- primaryExpr
    restExprs <- manyExprs ['*', '/'] primaryExpr
    return $ combineExprs leftExpr restExprs

  primaryExpr = choice [evaLiteral, parenExpr]
  parenExpr = char '(' *> spaces *> evaExpression <* spaces <* char ')'

  opers ops = toOp <$> (spaces *> choice (char <$> ops) <* spaces)
  manyExprs ops expr = many $ (\op a b -> EvaBinaryExpr op b a) <$> try (opers ops) <*> expr
  combineExprs = foldl (&)

evaLiteral :: Parser EvaAst
evaLiteral = choice [evaNumber, evaString]

evaNumber :: Parser EvaAst
evaNumber = (read >>> EvaNumber) <$> many1 digit

evaString :: Parser EvaAst
evaString = (T.pack >>> EvaString) <$> between (char '"') (char '"') (many $ noneOf "\"")
