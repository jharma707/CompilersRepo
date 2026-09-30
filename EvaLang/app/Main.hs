{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Parser
import Backend.TreeWalkInterpreter

interpret source = do
  ast <- parseEva source
  return $ interpretEvaAst ast

main :: IO ()
main = case parseEva "if ( 3 + 4 > 7 ) if (1) 3; else 0; else { 4; }" of
         (Left err) -> print err
         (Right res) -> print res
