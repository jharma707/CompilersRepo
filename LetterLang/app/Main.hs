{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Parser
import Backend.TreeWalkInterpreter
import Printers.SExpr

interpret source = do
  ast <- parseLetter source
  return $ interpretLetterAst ast

main :: IO ()
main = case parseLetter "addNums((fn x -> x * 10)(2), clamp10(11)); " of
         (Left err) -> print err
         (Right res) -> print $ SExprPrinter res
