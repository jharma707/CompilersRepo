module Parser (parseEva) where

import Ast

import Control.Arrow
import qualified Data.Text as T
import Data.Function
import Text.Parsec
import Text.Parsec.Text (Parser)

keywords =
  [ "let"
  , "if"
  , "else"
  , "true"
  , "false"
  , "null"
  , "while"
  , "do"
  , "for"
  ]

parseEva :: T.Text -> Either ParseError EvaAst
parseEva = parse evaProgram ""

evaProgram :: Parser EvaAst
evaProgram = evaStatementList

keyword str = string str <* (notFollowedBy evaValidIdChars)
evaKeywordLet   = keyword "let"
evaKeywordIf    = keyword "if"
evaKeywordElse  = keyword "else"
evaKeywordTrue  = keyword "true"
evaKeywordFalse = keyword "false"
evaKeywordNull  = keyword "null"
evaKeywordWhile = keyword "while"
evaKeywordDo    = keyword "do"
evaKeywordFor   = keyword "for"

evaStatementList = EvaStatements <$> many1 evaStatement

evaBlock = EvaBlock <$> (between start end (many evaStatement)) where
  start = char '{' <* spaces
  end   = char '}' <* spaces

evaEmptyStatement = const EvaEmptyStatement <$> (char ';' <* spaces)

evaStatement = choice
  [ evaLetStatement
  , evaIfStatement
  , evaWhileStatement
  , evaForStatement
  , evaDoWhileStatement
  , evaExprStatement
  , evaBlock
  , evaEmptyStatement
  ]

evaExprStatement = evaExpr <* spaces <* char ';' <* spaces

evaExpr = evaAssignment

evaLetStatement = evaLetBindings <* char ';' <* spaces
evaLetBindings = EvaLetDeclaration <$> (evaKeywordLet *> spaces *> (sepBy1 evaVarDeclaration (char ',' <* spaces)))
evaVarDeclaration = (,) <$> evaVar <*> (optionMaybe evaVarInitializer)

evaVar = evaIdentifier <* spaces
evaVarInitializer = char '=' *> spaces *> evaAssignment <* spaces
evaAssignment = simpleAssignment <|> evaBinary where
 simpleAssignment  = try $ EvaAssign <$> evaVar <*> evaVarInitializer

evaIfStatement = do
  condition  <- evaKeywordIf *> spaces *> char '(' *> spaces *> evaExpr <* spaces <* char ')' <* spaces
  consequent <- evaStatement <* spaces
  alternate  <- optionMaybe $ evaKeywordElse *> spaces *> evaStatement <* spaces
  return $ EvaIfStatement condition consequent alternate

evaWhileStatement = do
  _         <- evaKeywordWhile <* spaces <* char '(' <* spaces
  condition <- evaExpr <* spaces <* char ')' <* spaces
  block     <- evaBlock <* spaces
  return $ EvaWhileLoop condition block

evaDoWhileStatement = do
  _         <- evaKeywordDo <* spaces
  block     <- evaBlock <* spaces
  _         <- evaKeywordWhile <* spaces <* char '(' <* spaces
  condition <- evaExpr <* spaces <* char ')' <* spaces <* char ';' <* spaces
  return $ EvaDoWhileLoop condition block

evaForStatement = do
  _                <- evaKeywordFor <* spaces <* char '(' <* spaces
  maybeAssignments <- (optionMaybe (evaLetBindings <|> sequenceExpr)) <* char ';' <* spaces
  maybeCondition   <- (optionMaybe evaExpr) <* spaces <* char ';' <* spaces
  maybeIncrementer <- (optionMaybe evaExpr) <* spaces <* char ')' <* spaces
  block            <- evaBlock <* spaces
  return $ EvaForLoop maybeAssignments maybeCondition maybeIncrementer block where
    sequenceExpr = EvaSequenceExpr <$> (sepBy1 evaExpr (spaces *> char ',' <* spaces))

evaBinary = boolOr where
  binary ops subexpr = do
    leftExpr  <- subexpr <* spaces
    restExprs <- many $ leftAssociative <$> ((ops <* spaces) >>= toBinaryOp) <*> (subexpr <* spaces)
    return $ foldl (&) leftExpr restExprs

  leftAssociative = EvaBinaryExpr >>> flip
  boolOr   = binary (opers ["||"]) boolAnd
  boolAnd  = binary (opers ["&&"]) equality
  equality = binary (opers ["==", "!="]) relation
  relation = binary (opers ["<=", ">=", "<", ">"]) term
  term     = binary (opers ["+", "-"]) factor
  factor   = binary (opers ["*", "/"]) evaUnary

evaUnary = unary <|> evaPrimary where
  unary = EvaUnaryExpr <$> ((operators <* spaces) >>= toUnaryOp) <*> evaUnary
  operators = opers ["-", "+", "!"]

evaPrimary = choice [evaLiteral, evaIdentifier, evaParen]
evaParen = char '(' *> spaces *> evaExpr <* spaces <* char ')'

evaIdentifier = do
  identifier <- (:) <$> letter <*> many evaValidIdChars
  if identifier `elem` keywords
  then parserFail $ show identifier <> " is a reserved keyword and can't be used as an identifier."
  else return $ EvaIdentifier $ T.pack identifier

evaValidIdChars = choice [char '_', letter, digit]

evaLiteral = choice [evaNumber, evaString, evaBool, evaNull]
evaNumber  = (read >>> EvaNumber) <$> many1 digit
evaString  = (T.pack >>> EvaString) <$> between (char '"') (char '"') (many $ noneOf "\"")
evaBool    = EvaBool <$> (choice [evaKeywordTrue, evaKeywordFalse] >>= toBool)
evaNull    = const EvaNull <$> evaKeywordNull

-- helper functions
opers ops = choice $ try . string <$> ops

toBinaryOp :: String -> Parser BinaryOp
toBinaryOp "+"  = return Plus
toBinaryOp "-"  = return Minus
toBinaryOp "*"  = return Multiply
toBinaryOp "/"  = return Divide
toBinaryOp "<=" = return LessEq
toBinaryOp ">=" = return GreaterEq
toBinaryOp "<"  = return Less
toBinaryOp ">"  = return Greater
toBinaryOp "==" = return Equality
toBinaryOp "&&" = return And
toBinaryOp "||" = return Or
toBinaryOp oper = parserFail $ "unexpected binary expression: " <> oper

toBool :: String -> Parser Bool
toBool "true"  = return True
toBool "false" = return False
toBool literal = parserFail $ "unexpected boolean expression: " <> literal

toUnaryOp :: String -> Parser UnaryOp
toUnaryOp "-"  = return Negative
toUnaryOp "+"  = return Positive
toUnaryOp "!"  = return Negation
toUnaryOp oper = parserFail $ "unexpected unary expression: " <> oper
