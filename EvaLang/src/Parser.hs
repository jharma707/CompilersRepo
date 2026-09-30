module Parser (parseEva) where

import Ast

import Control.Arrow
import qualified Data.Text as T
import Data.Function
import Text.Parsec
import Text.Parsec.Text (Parser)

keywords = ["let", "if", "else"]

parseEva :: T.Text -> Either ParseError EvaAst
parseEva = parse evaProgram ""

evaProgram :: Parser EvaAst
evaProgram = evaStatementList

keyword str = string str >> (notFollowedBy evaValidIdChars)
evaKeywordLet   = keyword "let"
evaKeywordIf    = keyword "if"
evaKeywordElse  = keyword "else"

evaStatementList = EvaStatements <$> many1 evaStatement

evaBlock = EvaBlock <$> (between start end (many evaStatement)) where
  start = char '{' <* spaces
  end   = char '}' <* spaces

evaEmptyStatement = const EvaEmptyStatement <$> (char ';' <* spaces)

evaStatement = choice
  [ evaVariableStatement
  , evaIfStatement
  , evaExprStatement
  , evaBlock
  , evaEmptyStatement
  ]

evaExprStatement = evaExpr <* spaces <* char ';' <* spaces
evaExpr = evaAssignment

evaVariableStatement = do
  _       <- evaKeywordLet <* spaces
  varDecs <- sepBy1 varDeclaration (char ',' <* spaces)
  _       <- char ';' <* spaces
  return $ EvaLetDeclaration varDecs where
    varDeclaration = (,) <$> evaVar <*> (optionMaybe evaVarInitializer)

evaVar = T.pack <$> evaIdentifier <* spaces
evaVarInitializer = char '=' *> spaces *> evaAssignment <* spaces
evaAssignment = evaArithmetic <|> evaSimpleAssignment where
  evaSimpleAssignment = EvaAssign <$> evaVar <*> evaVarInitializer

evaIfStatement = do
  condition  <- evaKeywordIf *> spaces *> char '(' *> spaces *> evaExpr <* spaces <* char ')' <* spaces
  consequent <- evaStatement <* spaces
  alternate  <- optionMaybe $ evaKeywordElse *> spaces *> evaStatement <* spaces
  return $ EvaIfStatement condition consequent alternate

evaArithmetic = term where
  arithmetic ops subexpr = do
    leftExpr  <- subexpr <* spaces
    restExprs <- many $ leftAssociative <$> ((try ops <* spaces) >>= toBinaryOp) <*> (subexpr <* spaces)
    return $ foldl (&) leftExpr restExprs

  leftAssociative = EvaBinaryExpr >>> flip
  term   = arithmetic termOps factor
  factor = arithmetic factorOps evaUnary

evaUnary = unaryOp <|> evaPrimary where
  unaryOp = EvaUnaryExpr <$> (try (unaryOps <* spaces) >>= toUnaryOp) <*> evaUnary

evaPrimary = choice [evaLiteral, evaParen]
evaParen = char '(' *> spaces *> evaExpr <* spaces <* char ')'

evaIdentifier = do
  identifier <- (:) <$> letter <*> many evaValidIdChars
  if identifier `elem` keywords
  then parserFail $ show identifier <> " is a reserved keyword and can't be used as an identifier."
  else return identifier

evaLiteral = choice [evaNumber, evaString]
evaValidIdChars = choice [char '_', letter, digit]
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
