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

evaStatementList = EvaStatements <$> many1 evaStatement

evaBlock = EvaBlock <$> (between start end (many evaStatement)) where
  start = char '{' <* spaces
  end   = char '}' <* spaces

evaEmptyStatement = const EvaEmptyStatement <$> (char ';' <* spaces)

evaStatement = choice
  [ evaExprStatement
  , evaBlock
  , evaEmptyStatement
  ]

evaExprStatement = evaExpr <* spaces <* char ';' <* spaces

evaExpr = evaAssignment

evaAssignment = evaArithmetic <|> assignExpr where
  assignExpr = (T.pack >>> EvaAssign) <$> (evaIdentifier <* assignOp) <*> evaAssignment
  assignOp = spaces <* char '=' <* spaces

evaArithmetic = term where
  arithmetic ops subexpr = do
    leftExpr  <- subexpr <* spaces
    restExprs <- many $ leftAssociative <$> ((try ops <* spaces) >>= toBinaryOp) <*> (subexpr <* spaces)
    return $ foldl (&) leftExpr restExprs

  leftAssociative op = flip $ EvaBinaryExpr op
  term   = arithmetic termOps factor
  factor = arithmetic factorOps evaUnary

evaUnary = unaryOp <|> evaPrimary where
  unaryOp = EvaUnaryExpr <$> (try (unaryOps <* spaces) >>= toUnaryOp) <*> evaUnary

evaPrimary = choice [evaLiteral, evaParen]
evaParen = char '(' *> spaces *> evaExpr <* spaces <* char ')'

evaLiteral = choice [evaNumber, evaString]
evaIdentifier = (:) <$> letter <*> many (choice [char '_', letter, digit])
evaNumber = (read >>> EvaNumber) <$> many1 digit
evaString = (T.pack >>> EvaString) <$> between (char '"') (char '"') (many $ noneOf "\"")

-- helper functions
termOps   = choice $ char <$> "+-"
factorOps = choice $ char <$> "*/"
unaryOps  = choice $ char <$> "-"

toBinaryOp :: Char -> Parser BinaryOp
toBinaryOp '+'  = return Plus
toBinaryOp '-'  = return Minus
toBinaryOp '*'  = return Multiply
toBinaryOp '/'  = return Divide
toBinaryOp oper = parserFail $ "unexpected binary expression: " <> [oper]

toUnaryOp :: Char -> Parser UnaryOp
toUnaryOp '-'  = return Negative
toUnaryOp oper = parserFail $ "unexpected unary expression: " <> [oper]
