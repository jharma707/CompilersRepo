module Main (main) where

import Parser

main :: IO ()
main = print $ parseEva "\"Eva\"; 42;  "
