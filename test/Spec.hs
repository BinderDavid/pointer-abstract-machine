module Main where

import Test.Hspec

import AbstractMachine
import AbstractMachine (topOfStack, emptyStack)

main :: IO ()
main = hspec $ do
    describe "Stacks" $ do
        it "Top of empty stack is the null pointer" $
          topOfStack emptyStack `shouldBe` 0

