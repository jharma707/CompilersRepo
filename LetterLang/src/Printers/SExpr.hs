{-# LANGUAGE OverloadedStrings #-}

module Printers.SExpr where

import Ast

newtype SExprPrinter = SExprPrinter LetterAst

instance Show SExprPrinter where
  show (SExprPrinter ast) =
    case ast of
      (LetterIdentifier iden) -> show iden
      _ -> ""
