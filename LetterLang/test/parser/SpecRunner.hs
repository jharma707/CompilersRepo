{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parser
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "when parsing Letter code" $ do
    it "should parse successfully" $ do
      1 + 1 `shouldBe` 2
