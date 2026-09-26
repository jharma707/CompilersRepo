module Parser (parseEva) where

import Ast

import Control.Arrow
import Text.Parsec
import Text.Parsec.String (Parser)

parseEva :: String -> Either ParseError EvaAst
parseEva = parse evaProgram ""

evaProgram :: Parser EvaAst
evaProgram = evaStatementList

evaStatementList :: Parser EvaAst
evaStatementList = EvaStatements <$> many1 evaStatement

evaStatement :: Parser EvaAst
evaStatement = evaExpressionStatement

evaExpressionStatement :: Parser EvaAst
evaExpressionStatement = evaLiteral <* spaces <* char ';' <* spaces

evaLiteral :: Parser EvaAst
evaLiteral = choice [evaNumber, evaString]

evaNumber :: Parser EvaAst
evaNumber = (read >>> EvaNumber) <$> many1 digit

evaString :: Parser EvaAst
evaString = EvaString <$> between (char '"') (char '"') (many $ noneOf "\"")
