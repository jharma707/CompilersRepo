module Parser (parseLetter) where

import Ast

import Control.Arrow
import Control.Applicative ((<**>))
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
  , "class"
  , "extends"
  , "this"
  , "super"
  , "new"
  ]

parseLetter :: T.Text -> Either ParseError LetterAst
parseLetter = parse letterProgram ""

letterProgram :: Parser LetterAst
letterProgram = LetterProgram <$> (whitespace *> letterStatementList)

keyword str = lexeme $ string' str <* (notFollowedBy letterValidIdChars)
letterKeywordLet     = keyword "let"
letterKeywordIf      = keyword "if"
letterKeywordElse    = keyword "else"
letterKeywordTrue    = keyword "true"
letterKeywordFalse   = keyword "false"
letterKeywordNull    = keyword "null"
letterKeywordWhile   = keyword "while"
letterKeywordDo      = keyword "do"
letterKeywordFor     = keyword "for"
letterKeywordDef     = keyword "def"
letterKeywordReturn  = keyword "return"
letterKeywordClass   = keyword "class"
letterKeywordExtends = keyword "extends"
letterKeywordThis    = keyword "this"
letterKeywordSuper   = keyword "super"
letterKeywordNew     = keyword "new"

letterStatementList = LetterStatements <$> many1 letterStatement
letterBlock = LetterBlock <$> (between openBrace closeBrace (many letterStatement))
letterEmptyStatement = LetterEmptyStatement <$ semicolon

letterStatement = choice
  [ letterLetStatement
  , letterIfStatement
  , letterWhileStatement
  , letterForStatement
  , letterDoWhileStatement
  , letterReturnStatement
  , letterFunctionDeclaration
  , letterClassDeclaration
  , letterExprStatement
  , letterBlock
  , letterEmptyStatement
  ]

letterExprStatement = letterExpr <* semicolon
letterExpr = letterAssignment

letterLetStatement = letterLetBindings <* semicolon
letterLetBindings = LetterLetDeclaration <$> (letterKeywordLet *> (sepBy1 letterVarDeclaration comma))

letterVarDeclaration = (,) <$> letterIdentifier <*> (optionMaybe letterVarInitializer)
letterVarInitializer = assign *> letterAssignment

letterAssignment = simpleAssignment <|> letterBinary where
 simpleAssignment = do
   results <- try $ (,) <$> letterLeftExpr <*> letterVarInitializer
   case fst results of
     (LetterIdentifier _)     -> return $ (uncurry LetterAssign) results
     (LetterMemberExpr _ _ _) -> return $ (uncurry LetterAssign) results
     _                     -> parserFail "Invalid left-hand side in assignment expression."

letterIfStatement = do
  condition  <- letterKeywordIf *> letterParen
  consequent <- letterStatement
  alternate  <- optionMaybe $ letterKeywordElse *> letterStatement
  return $ LetterIfStatement condition consequent alternate

letterWhileStatement = LetterWhileLoop <$> (letterKeywordWhile *> letterParen) <*> letterBlock

letterDoWhileStatement = do
  block     <- letterKeywordDo *> letterBlock
  condition <- letterKeywordWhile *> letterParen <* semicolon
  return $ LetterDoWhileLoop condition block

letterForStatement = do
  _                <- letterKeywordFor <* openParen
  maybeAssignments <- (optionMaybe (letterLetBindings <|> sequenceExpr)) <* semicolon
  maybeCondition   <- (optionMaybe letterExpr) <* semicolon
  maybeIncrementer <- (optionMaybe letterExpr) <* closeParen
  block            <- letterBlock
  return $ LetterForLoop maybeAssignments maybeCondition maybeIncrementer block where
    sequenceExpr = LetterSequenceExpr <$> (sepBy1 letterExpr comma)

letterReturnStatement = LetterReturnStatement <$> (letterKeywordReturn *> (optionMaybe letterExpr) <* semicolon)

letterFunctionDeclaration = do
  name   <- letterKeywordDef *> letterIdentifier
  params <- between openParen closeParen $ sepBy letterIdentifier comma
  body   <- letterBlock
  return $ LetterFunctionDeclaration name params body

letterClassDeclaration
  =   LetterClassDeclaration
  <$> (letterKeywordClass *> letterIdentifier)
  <*> (optionMaybe (letterKeywordExtends *> letterIdentifier))
  <*> letterBlock

letterBinary = boolOr where
  binary ops subexpr = do
    leftExpr  <- subexpr
    restExprs <- many $ leftAssociative <$> (ops >>= toBinaryOp) <*> subexpr
    return $ foldl (&) leftExpr restExprs

  leftAssociative = LetterBinaryExpr >>> flip
  boolOr   = binary (opers ["||"]) boolAnd
  boolAnd  = binary (opers ["&&"]) equality
  equality = binary (opers ["==", "!="]) relation
  relation = binary (opers ["<=", ">=", "<", ">"]) term
  term     = binary (opers ["+", "-"]) factor
  factor   = binary (opers ["*", "/"]) letterUnary

letterUnary = unary <|> letterLeftExpr where
  unary = LetterUnaryExpr <$> (operators >>= toUnaryOp) <*> letterUnary
  operators = opers ["-", "+", "!"]

letterLeftExpr = letterSuper <|> (letterCallMemberExpr letterPrimary)

letterMemberProperty   = flip (LetterMemberExpr False) <$> (property *> letterIdentifier)
letterComputedProperty = flip (LetterMemberExpr True)  <$> (between memberOpen memberClose letterExpr)

letterArguments = between openParen closeParen (sepBy letterExpr comma)
letterCallExpr = flip LetterCallExpr <$> letterArguments

letterCallMemberExpr = letterChainHelper [letterMemberProperty, letterComputedProperty, letterCallExpr]
letterMemberExpr     = letterChainHelper [letterMemberProperty, letterComputedProperty]

letterSuper = letterCallMemberExpr $ (LetterSuper <$ letterKeywordSuper) <**> letterCallExpr
letterNew = LetterNew <$> (letterKeywordNew *> (letterMemberExpr letterPrimary)) <*> letterArguments

letterChainHelper choices objectP = do
  object     <- objectP
  properties <- many $ choice choices
  return $ foldl (&) object properties

letterPrimary = choice [letterThis, letterNew, letterLiteral, letterIdentifier, letterParen]
letterParen = between openParen closeParen letterExpr

letterIdentifier = do
  identifier <- lexeme $ (:) <$> letter <*> many letterValidIdChars
  if identifier `elem` keywords
  then parserFail $ show identifier <> " is a reserved keyword and can't be used as an identifier."
  else return $ LetterIdentifier $ T.pack identifier

letterThis = LetterThisExpr <$ letterKeywordThis

letterValidIdChars = char '_' <|> alphaNum

letterLiteral = lexeme $ choice [letterNumber, letterString, letterBool, letterNull]
letterNumber  = (read >>> LetterNumber) <$> many1 digit
letterString  = (T.pack >>> LetterString) <$> between (char '"') (char '"') (many $ noneOf "\"")
letterBool    = LetterBool <$> (choice [letterKeywordTrue, letterKeywordFalse] >>= toBool)
letterNull    = LetterNull <$ letterKeywordNull

letterLineComment = string' "//" *> (skipMany (noneOf "\r\n")) *> (choice [eof, void endOfLine])

whitespace = skipMany $ choice [skipMany1 space, letterLineComment]

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
