{-# LANGUAGE OverloadedStrings #-}

module Printers.SExpr where

import Ast

newtype SExprPrinter = SExprPrinter EvaAst

instance Show SExprPrinter where
  show (SExprPrinter ast) =
    case ast of
      (EvaIdentifier iden) -> show iden
      _ -> ""
