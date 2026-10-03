{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Parser
import Backend.TreeWalkInterpreter

interpret source = do
  ast <- parseEva source
  return $ interpretEvaAst ast

main :: IO ()
main = case parseEva "!(x + 4) || -+!y * 2;" of
         (Left err) -> print err
         (Right res) -> print res
