module Main where

import Test.Hspec

import AbstractMachine.Machine
import AbstractMachine.Stack

main :: IO ()
main = hspec $ do
    describe "Stacks" $ do
        it "Top of empty stack is the null pointer" $
          topOfStack emptyStack `shouldBe` 0

