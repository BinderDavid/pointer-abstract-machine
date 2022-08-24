module AbstractMachine.Stack where

import Syntax

-------------------------------------------------------------------------------
-- The stack
-------------------------------------------------------------------------------

-- | A pointer into the stack
type Pointer = Int

-- | The stack contains bindings for both terms and continuations.
data StackBinding where
  TermBinding :: Term -> Pointer -> StackBinding
  ContinuationBinding :: Continuation -> Pointer -> StackBinding

newtype Stack = MkStack { unStack :: [(Var, StackBinding)] }

emptyStack :: Stack
emptyStack = MkStack []

-- | Get a pointer to the top of the stack.
topOfStack :: Stack -> Pointer
topOfStack stack = length (unStack stack)

-- | Pop from the top of the stack until you reach the pointer.
restrictStack :: Stack -> Pointer -> Stack
restrictStack stack pt = MkStack $ reverse $ take (pt + 1) $ reverse stack'
  where
    stack' = unStack stack

-- | Push a binding on top of the stack.
extendStack :: Stack -> Var -> StackBinding -> Stack
extendStack stack var bnd = MkStack $ (var, bnd) : unStack stack

lookupStack :: Pointer -> Stack -> Var -> Either String StackBinding
lookupStack pt stack = lookupStack' (restrictStack stack pt)
  where
    lookupStack' :: Stack -> Var -> Either String StackBinding
    lookupStack' (MkStack []) var = Left ("Variable " <> var <> " not in stack.")
    lookupStack' (MkStack ((v,bnd):stack')) v' | v == v' = Right bnd
                                               | otherwise = lookupStack' (MkStack stack') v'