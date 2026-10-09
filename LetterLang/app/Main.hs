{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Parser
import Backend.TreeWalkInterpreter
import Printers.SExpr

interpret source = do
  ast <- parseLetter source
  return $ interpretLetterAst ast

main :: IO ()
main = case parseLetter "if (true) { 45; 34; }" of
         (Left err) -> print err
         (Right res) -> print $ SExprPrinter res
