{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Parser

main :: IO ()
main = print $ parseEva "\"Eva\"; 42; \n42; { \"jordan\"; 69;; { (2 + 3) * 5; 4 + 1 * 3 + 0; 1 + 2 + 3; } } "
