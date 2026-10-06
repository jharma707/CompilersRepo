{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Parser
import Backend.TreeWalkInterpreter

interpret source = do
  ast <- parseLetter source
  return $ interpretLetterAst ast

main :: IO ()
main = case parseLetter "new A(8).x; super(3, 4).b[3];" of
         (Left err) -> print err
         (Right res) -> print res
