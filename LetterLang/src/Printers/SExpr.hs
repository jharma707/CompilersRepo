module Printers.SExpr where

import Ast
import qualified Data.Text as T
import Data.List (intersperse)
import Control.Arrow

newtype SExprPrinter = SExprPrinter { getAst :: LetterAst }

instance Show SExprPrinter where
  show (SExprPrinter letterAst) = printSExpr letterAst where
    sexpr expr = "(" <> expr <> ")"
    combineStrings = intersperse " " >>> concat
    sexprSeq = combineStrings >>> sexpr

    printSeq = evalSeq >>> combineStrings
    evalSeq  = fmap printSExpr

    printMaybeNode (Just astNode) = printSExpr astNode
    printMaybeNode Nothing        = sexpr ""

    printSExpr (LetterProgram program) = printSExpr program
    printSExpr (LetterNumber n) = show n
    printSExpr (LetterString str) = show str
    printSExpr (LetterBool b) = if b then "true" else "false"
    printSExpr LetterNull = "null"
    printSExpr (LetterStatements stmts) = printSeq stmts
    printSExpr (LetterBlock stmts) =
      sexprSeq ["begin", printSeq stmts]
    printSExpr LetterEmptyStatement = ""
    printSExpr (LetterReturnStatement maybeRet) =
      sexprSeq ["return", printMaybeNode maybeRet]
    printSExpr (LetterBinaryExpr op v1 v2) =
      sexprSeq [show op, printSExpr v1, printSExpr v2]
    printSExpr (LetterUnaryExpr op v) =
      sexprSeq [show op, printSExpr v]
    printSExpr (LetterMemberExpr isComputed obj member) =
      sexprSeq [if isComputed then "[]" else ".", printSExpr obj, printSExpr member]
    printSExpr (LetterCallExpr f args) =
      sexprSeq [printSExpr f, printSeq args]
    printSExpr LetterThisExpr = "this"
    printSExpr (LetterLambdaExpr args body) =
      sexprSeq ["lambda", sexpr (printSeq args), printSExpr body]
    printSExpr LetterSuper = "super"
    printSExpr (LetterNew member args) =
      sexprSeq ["new", sexprSeq [printSExpr member, printSeq args]]
    printSExpr (LetterAssign l r) =
      sexprSeq ["=", printSExpr l, printSExpr r]
    printSExpr (LetterIdentifier iden) = T.unpack iden
    printSExpr (LetterLetDeclaration bindings) =
      let printBinding (name, maybeV) = sexprSeq [printSExpr name, printMaybeNode maybeV]
       in sexprSeq ["let", combineStrings (printBinding <$> bindings)]
    printSExpr (LetterIfStatement cond consequent maybeAlternate) =
      sexprSeq ["if", printSExpr cond, printSExpr consequent, printMaybeNode maybeAlternate]
    printSExpr (LetterWhileLoop cond body) =
      sexprSeq $ ["while"] <> (evalSeq [cond, body])
    printSExpr (LetterDoWhileLoop cond body) =
      sexprSeq $ ["do-while"] <> (evalSeq [cond, body])
    printSExpr (LetterForLoop maybeInit maybeCond maybeIncrement body) =
      sexprSeq
        [ "for"
        , sexprSeq (printMaybeNode <$> [maybeInit, maybeCond, maybeIncrement])
        , printSExpr body
        ]
    printSExpr (LetterSequenceExpr seqExpr) = printSeq seqExpr
    printSExpr (LetterFunctionDeclaration name params body) =
      sexprSeq ["def", sexpr (printSeq (name : params)), printSExpr body]
    printSExpr (LetterConstructor params body) =
      sexprSeq ["constructor", sexpr (printSeq params), printSExpr body]
    printSExpr (LetterClassDeclaration name fields maybeParentClass maybeBody) =
      sexprSeq
        [ "class"
        , printSExpr name
        , sexpr (printSeq fields)
        , printMaybeNode maybeParentClass
        , printMaybeNode maybeBody
        ]
