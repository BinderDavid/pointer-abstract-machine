module Spec.Stack (stackSpec) where

import Test.Hspec

import AbstractMachine.Stack qualified as ST

stackSpec :: Spec
stackSpec = do
    it "Top of empty stack is the null pointer" $
      ST.topOfStack ST.emptyStack `shouldBe` 0