{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Parser
import Backend.TreeWalkInterpreter

interpret source = do
  ast <- parseEva source
  return $ interpretEvaAst ast

main :: IO ()
main = case parseEva "for (i, j=1; i < 10; i = i + 1) { 42 + 27; }" of
         (Left err) -> print err
         (Right res) -> print res
