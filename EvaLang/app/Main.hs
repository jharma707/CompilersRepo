{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Parser
import Backend.TreeWalkInterpreter

interpret source = do
  ast <- parseEva source
  return $ interpretEvaAst ast

main :: IO ()
main = case parseEva "if ( 3 + 4 && 3 == false ) 3;" of
         (Left err) -> print err
         (Right res) -> print res
