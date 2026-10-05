module Parser (parseEva) where

import Ast

import Control.Arrow
import Control.Monad (void)
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
  , "def"
  , "return"
  ]

parseEva :: T.Text -> Either ParseError EvaAst
parseEva = parse evaProgram ""

evaProgram :: Parser EvaAst
evaProgram = EvaProgram <$> (whitespace *> evaStatementList)

keyword str = lexeme $ string' str <* (notFollowedBy evaValidIdChars)
evaKeywordLet    = keyword "let"
evaKeywordIf     = keyword "if"
evaKeywordElse   = keyword "else"
evaKeywordTrue   = keyword "true"
evaKeywordFalse  = keyword "false"
evaKeywordNull   = keyword "null"
evaKeywordWhile  = keyword "while"
evaKeywordDo     = keyword "do"
evaKeywordFor    = keyword "for"
evaKeywordDef    = keyword "def"
evaKeywordReturn = keyword "return"

evaStatementList = EvaStatements <$> many1 evaStatement
evaBlock = EvaBlock <$> (between openBrace closeBrace (many evaStatement))
evaEmptyStatement = const EvaEmptyStatement <$> semicolon

evaStatement = choice
  [ evaLetStatement
  , evaIfStatement
  , evaWhileStatement
  , evaForStatement
  , evaDoWhileStatement
  , evaFunctionDeclaration
  , evaReturnStatement
  , evaExprStatement
  , evaBlock
  , evaEmptyStatement
  ]

evaExprStatement = evaExpr <* semicolon
evaExpr = evaAssignment

evaLetStatement = evaLetBindings <* semicolon
evaLetBindings = EvaLetDeclaration <$> (evaKeywordLet *> (sepBy1 evaVarDeclaration comma))

evaVarDeclaration = (,) <$> evaIdentifier <*> (optionMaybe evaVarInitializer)
evaVarInitializer = assign *> evaAssignment

evaAssignment = simpleAssignment <|> evaBinary where
 simpleAssignment = do
   results <- try $ do
     leftExpr <- evaLeftExpr
     initExpr <- evaVarInitializer
     return (leftExpr, initExpr)

   case fst results of
     (EvaIdentifier _)     -> return $ (uncurry EvaAssign) results
     (EvaMemberExpr _ _ _) -> return $ (uncurry EvaAssign) results
     _                     -> parserFail "Invalid left-hand side in assignment expression."

evaIfStatement = do
  condition  <- evaKeywordIf *> evaParen
  consequent <- evaStatement
  alternate  <- optionMaybe $ evaKeywordElse *> evaStatement
  return $ EvaIfStatement condition consequent alternate

evaWhileStatement = EvaWhileLoop <$> (evaKeywordWhile *> evaParen) <*> evaBlock

evaDoWhileStatement = do
  block     <- evaKeywordDo *> evaBlock
  condition <- evaKeywordWhile *> evaParen <* semicolon
  return $ EvaDoWhileLoop condition block

evaForStatement = do
  _                <- evaKeywordFor <* openParen
  maybeAssignments <- (optionMaybe (evaLetBindings <|> sequenceExpr)) <* semicolon
  maybeCondition   <- (optionMaybe evaExpr) <* semicolon
  maybeIncrementer <- (optionMaybe evaExpr) <* closeParen
  block            <- evaBlock
  return $ EvaForLoop maybeAssignments maybeCondition maybeIncrementer block where
    sequenceExpr = EvaSequenceExpr <$> (sepBy1 evaExpr comma)

evaReturnStatement = EvaReturnStatement <$> (evaKeywordReturn *> (optionMaybe evaExpr) <* semicolon)

evaFunctionDeclaration = do
  name   <- evaKeywordDef *> evaIdentifier
  params <- between openParen closeParen $ sepBy evaIdentifier comma
  body   <- evaBlock
  return $ EvaFunctionDeclaration name params body

evaBinary = boolOr where
  binary ops subexpr = do
    leftExpr  <- subexpr
    restExprs <- many $ leftAssociative <$> (ops >>= toBinaryOp) <*> subexpr
    return $ foldl (&) leftExpr restExprs

  leftAssociative = EvaBinaryExpr >>> flip
  boolOr   = binary (opers ["||"]) boolAnd
  boolAnd  = binary (opers ["&&"]) equality
  equality = binary (opers ["==", "!="]) relation
  relation = binary (opers ["<=", ">=", "<", ">"]) term
  term     = binary (opers ["+", "-"]) factor
  factor   = binary (opers ["*", "/"]) evaUnary

evaUnary = unary <|> evaLeftExpr where
  unary = EvaUnaryExpr <$> (operators >>= toUnaryOp) <*> evaUnary
  operators = opers ["-", "+", "!"]

evaLeftExpr = evaMemberExpr

evaMemberExpr = do
  object     <- evaPrimary
  properties <- many $ choice [memberProperty, computedProperty, callExpr]
  return $ foldl (&) object properties where
    memberProperty   = flip (EvaMemberExpr False) <$> (property *> evaIdentifier)
    computedProperty = flip (EvaMemberExpr True)  <$> (between memberOpen memberClose evaExpr)
    callExpr         = flip EvaCallExpr <$> (between openParen closeParen (sepBy evaExpr comma))

evaPrimary = choice [evaLiteral, evaIdentifier, evaParen]
evaParen = between openParen closeParen evaExpr

evaIdentifier = do
  identifier <- lexeme $ (:) <$> letter <*> many evaValidIdChars
  if identifier `elem` keywords
  then parserFail $ show identifier <> " is a reserved keyword and can't be used as an identifier."
  else return $ EvaIdentifier $ T.pack identifier

evaValidIdChars = char '_' <|> alphaNum

evaLiteral = lexeme $ choice [evaNumber, evaString, evaBool, evaNull]
evaNumber  = (read >>> EvaNumber) <$> many1 digit
evaString  = (T.pack >>> EvaString) <$> between (char '"') (char '"') (many $ noneOf "\"")
evaBool    = EvaBool <$> (choice [evaKeywordTrue, evaKeywordFalse] >>= toBool)
evaNull    = const EvaNull <$> evaKeywordNull

evaLineComment = string' "//" *> (skipMany (noneOf "\r\n")) *> (choice [eof, void endOfLine])

whitespace = skipMany $ choice [skipMany1 space, evaLineComment]

-- helper functions
lexeme p = p <* whitespace
opers ops = lexeme $ choice $ try . string <$> ops
comma = lexeme $ char ','
openParen = lexeme $ char '('
closeParen = lexeme $ char ')'
semicolon = lexeme $ char ';'
assign = lexeme $ char '='
openBrace = lexeme $ char '{'
closeBrace = lexeme $ char '}'
property = lexeme $ char '.'
memberOpen = lexeme $ char '['
memberClose = lexeme $ char ']'

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
