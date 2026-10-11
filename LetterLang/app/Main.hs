{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Parser
import Backend.TreeWalkInterpreter
import Printers.SExpr

interpret source = do
  ast <- parseLetter source
  return $ interpretLetterAst ast

main :: IO ()
main = case parseLetter "super.x()[3];" of
         (Left err) -> print err
         (Right res) -> print $ SExprPrinter res
