module Spec.Heap ( heapSpec ) where

import Data.Either (isLeft)
import Test.Hspec

import AbstractMachine.Heap qualified as HP

heapSpec :: Spec
heapSpec = do
    it "Read on empty heap should return error" $
      HP.read (HP.emptyHeap :: HP.Heap ()) 0  `shouldSatisfy` isLeft