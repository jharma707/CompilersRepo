{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Parser

main :: IO ()
main = print $ parseEva "\"Eva\"; 42; \n42; { \"jordan\"; 69;; {} } "
