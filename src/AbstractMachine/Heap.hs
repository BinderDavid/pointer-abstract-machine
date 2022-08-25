module AbstractMachine.Heap where

type HeapPointer = Int


data Heap a =
  MkHeap { bumpPointer :: HeapPointer
         , heap :: [(HeapPointer, a)]
         } deriving (Show, Eq)


emptyHeap :: Heap a
emptyHeap = MkHeap
  { bumpPointer = 0
  , heap = []
  }

malloc :: Heap a -> a -> (HeapPointer, Heap a)
malloc MkHeap { bumpPointer, heap } val =
  (bumpPointer, MkHeap { bumpPointer = bumpPointer + 1, heap = (bumpPointer, val) : heap})

free :: Heap a -> HeapPointer -> Either String (Heap a)
free MkHeap { bumpPointer, heap } pt = do
  case lookup pt heap of
    Nothing -> Left ("Called \"free\" on invalid pointer: " <> show pt)
    Just _ -> pure MkHeap { bumpPointer = bumpPointer
                          , heap = filter ((/= pt) . fst) heap
                          }
  
read :: Heap a -> HeapPointer -> Either String a
read MkHeap { heap } pt = do
  case lookup pt heap of
    Nothing -> Left ("Called \"read\" on invalid pointer: " <> show pt)
    Just a -> pure a

write :: Heap a -> HeapPointer -> a -> Either String (Heap a)
write MkHeap { bumpPointer, heap } pt val = do
  if any ((== pt) . fst) heap
    then pure MkHeap { bumpPointer = bumpPointer
                     , heap = map (\(pt',val') -> if pt == pt' then (pt, val) else (pt',val')) heap
                     }
    else Left ("Called \"write\" on invalid pointer: " <> show pt)
  
  