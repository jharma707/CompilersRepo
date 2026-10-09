module Printers.SExpr where

import Ast
import Control.Arrow
import Data.List

newtype SExprPrinter = SExprPrinter { getAst :: LetterAst }

instance Show SExprPrinter where
  show (SExprPrinter letterAst) = printSExpr letterAst where
    combineStrings = intersperse " " >>> concat
    printSeq args = combineStrings $ printSExpr <$> args

    printBinding (name, (Just v))
      = "[" <> (printSExpr name) <> " " <> (printSExpr v) <> "]"
    printBinding (name, Nothing)
      = "[" <> (printSExpr name) <> "]"

    printLoopHeader (Just expr) = "[" <> (printSExpr expr) <> "]"
    printLoopHeader Nothing     = "[]"

    printParentClass (Just parent) = "(" <> printSExpr parent <> ")"
    printParentClass Nothing       = "null"

    printClassBody (Just body) = printSExpr body
    printClassBody Nothing     = "null"

    printSExpr (LetterProgram program) = printSExpr program
    printSExpr (LetterNumber n) = show n
    printSExpr (LetterString str) = show str
    printSExpr (LetterBool b) = if b then "true" else "false"
    printSExpr LetterNull = "null"
    printSExpr (LetterStatements stmts) =
      "(" <> (printSeq stmts) <> ")"
    printSExpr (LetterBlock stmts) =
      "(block " <> (printSeq stmts) <> ")"
    printSExpr LetterEmptyStatement = ""
    printSExpr (LetterReturnStatement (Just ret)) =
      "(return " <> (printSExpr ret) <> ")"
    printSExpr (LetterReturnStatement Nothing) = "(return)"
    printSExpr (LetterBinaryExpr op v1 v2) =
      "(" <> (show op) <> " " <> (printSExpr v1) <> " " <> (printSExpr v2) <> ")"
    printSExpr (LetterUnaryExpr op v) =
      "(" <> (show op) <> " " <> (printSExpr v) <> ")"
    printSExpr (LetterMemberExpr isComputed obj member) =
      "(" <> (if isComputed then "[]" else ".") <> " "
          <> (printSExpr obj) <> " " <> (printSExpr member) <> ")"
    printSExpr (LetterCallExpr f args) =
      "(" <> (printSExpr f) <> " " <> (printSeq args) <> ")"
    printSExpr LetterThisExpr = "this"
    printSExpr (LetterLambdaExpr args body) =
      "(lambda (" <> (printSeq args) <> ") " <> (printSExpr body) <> ")"
    printSExpr LetterSuper = "super"
    printSExpr (LetterNew member args) =
      "(new (" <> (printSExpr member) <> " " <> (printSeq args) <> "))"
    printSExpr (LetterAssign l r) =
      "(= " <> (printSExpr l) <> " " <> (printSExpr r) <> ")"
    printSExpr (LetterIdentifier iden) = show iden
    printSExpr (LetterLetDeclaration bindings) =
      "(let (" <> (combineStrings (printBinding <$> bindings)) <> "))"
    printSExpr (LetterIfStatement cond consequent (Just alternate)) =
      "(if " <> (printSExpr cond) <> " "
             <> (printSExpr consequent) <> " "
             <> (printSExpr alternate) <> ")"
    printSExpr (LetterIfStatement cond consequent Nothing) =
      "(if " <> (printSExpr cond) <> " " <> (printSExpr consequent) <> ")"
    printSExpr (LetterWhileLoop cond body) =
      "(while " <> (printSExpr cond) <> " " <> (printSExpr body) <> ")"
    printSExpr (LetterDoWhileLoop cond body) =
      "(do-while " <> (printSExpr cond) <> " " <> (printSExpr body) <> ")"
    printSExpr (LetterForLoop maybeInit maybeCond maybeIncrement body) =
      "(for (" <> (combineStrings (printLoopHeader <$> [maybeInit, maybeCond, maybeIncrement]))
               <> ")" <> (printSExpr body) <> ")"
    printSExpr (LetterSequenceExpr seqExpr) = printSeq seqExpr
    printSExpr (LetterFunctionDeclaration name params body) =
      "(def (" <> (printSExpr name) <> " " <> (printSeq params) <> ") "
               <> (printSExpr body) <> ")"
    printSExpr (LetterConstructor params body) =
      "(constructor (" <> (printSeq params) <> ") " <> (printSExpr body) <> ")"
    printSExpr (LetterClassDeclaration name fields maybeParentClass maybeBody) =
      "(class " <> (printSExpr name) <> " "
                <> "(" <> (printSeq fields) <> ")"
                <> (printParentClass maybeParentClass) <> " "
                <> (printClassBody maybeBody) <> ")"
