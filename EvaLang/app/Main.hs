{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Parser
import Backend.TreeWalkInterpreter

main :: IO ()
main = case (parseEva "4 * (1 + 2); ") of
         (Left err)  -> print err
         (Right res) -> print $ interpretEvaAst res
