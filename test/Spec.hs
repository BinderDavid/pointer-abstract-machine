module Main where

import Test.Hspec

import Spec.Stack (stackSpec)
import Spec.Heap (heapSpec)

main :: IO ()
main = hspec $ do
    describe "AbstractMachine.Stack" $ do
      stackSpec
    describe "AbstractMachine.Heap" $ do
      heapSpec

