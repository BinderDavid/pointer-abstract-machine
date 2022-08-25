module AbstractMachine.Stack where

import Syntax

-------------------------------------------------------------------------------
-- The stack
-------------------------------------------------------------------------------

-- | A pointer into the stack
type StackPointer = Int

-- | The stack contains bindings for both terms and continuations.
data StackBinding where
  TermBinding :: Term -> StackPointer -> StackBinding
  ContinuationBinding :: Continuation -> StackPointer -> StackBinding

newtype Stack = MkStack { unStack :: [(Var, StackBinding)] }

emptyStack :: Stack
emptyStack = MkStack []

-- | Get a pointer to the top of the stack.
topOfStack :: Stack -> StackPointer
topOfStack stack = length (unStack stack)

-- | Pop from the top of the stack until you reach the pointer.
restrictStack :: Stack -> StackPointer -> Stack
restrictStack stack pt = MkStack $ reverse $ take (pt + 1) $ reverse stack'
  where
    stack' = unStack stack

-- | Push a binding on top of the stack.
extendStack :: Stack -> Var -> StackBinding -> Stack
extendStack stack var bnd = MkStack $ (var, bnd) : unStack stack

lookupStack :: StackPointer -> Stack -> Var -> Either String StackBinding
lookupStack pt stack = lookupStack' (restrictStack stack pt)
  where
    lookupStack' :: Stack -> Var -> Either String StackBinding
    lookupStack' (MkStack []) var = Left ("Variable " <> var <> " not in stack.")
    lookupStack' (MkStack ((v,bnd):stack')) v' | v == v' = Right bnd
                                               | otherwise = lookupStack' (MkStack stack') v'

lookupTermBinding :: StackPointer -> Stack -> Var -> Either String (Term, StackPointer)
lookupTermBinding pt st var = do
  res <- lookupStack pt st var
  case res of
    TermBinding tm pt' -> pure (tm, pt')
    ContinuationBinding _ _ -> Left "Tried to lookup term but found continuation"

lookupContinuationBinding :: StackPointer -> Stack -> Var -> Either String (Continuation, StackPointer)
lookupContinuationBinding pt st var = do
  res <- lookupStack pt st var
  case res of
    ContinuationBinding cnt pt' -> pure (cnt, pt')
    TermBinding _ _ -> Left "Tried to lookup term but found continuation"
    