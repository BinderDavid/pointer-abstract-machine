{-# LANGUAGE GADTs #-}
module AbstractMachine where

import Syntax

-------------------------------------------------------------------------------
-- The Pointer Abstract Machine
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- The machine state
-------------------------------------------------------------------------------

data MachineState = MkMachineState Term Pointer Continuation Pointer Stack

lookupStack :: Pointer -> Stack -> Var -> Either String StackBinding
lookupStack pt stack = lookupStack' (restrictStack stack pt)
  where
    lookupStack' :: Stack -> Var -> Either String StackBinding
    lookupStack' (MkStack []) var = Left ("Variable " <> var <> " not in stack.")
    lookupStack' (MkStack ((v,bnd):stack')) v' | v == v' = Right bnd
                                               | otherwise = lookupStack' (MkStack stack') v'

embedCommand :: Command -> MachineState
embedCommand (Cut tm cnt) = MkMachineState tm 0 cnt 0 emptyStack

data ComputeStep where
    LambdaStep :: ComputeStep
    MuStep :: ComputeStep
    MuTildeStep :: ComputeStep
    TermVarStep :: ComputeStep
    ContinuationVarStep :: ComputeStep

computeStep :: MachineState -> Either String (MachineState, ComputeStep)
-- Reducing a cut between a lambda abstraction and a call stack
computeStep (MkMachineState (TmLambda x funbody) _p1 (CntCallStack funarg cont) p2 stack) =
    Right (MkMachineState funbody (topOfStack stack) cont p2 (MkStack $ (x, TermBinding funarg p2) : unStack stack), LambdaStep)
-- Reducing mu abstractions. This implements CBV since they are evaluated before
-- mu-tilde abstractions.
computeStep (MkMachineState (TmMu x (Cut tm cont)) _p1 cont' p2 stack) =
    Right (MkMachineState tm (topOfStack stack) cont (topOfStack stack) (extendStack stack x (ContinuationBinding cont' p2)), MuStep)
-- Reducing tilde mu abstractions
computeStep (MkMachineState tm' p1 (CntMu x (Cut tm cnt)) _p2 stack) =
    Right (MkMachineState tm (topOfStack stack) cnt (topOfStack stack) (extendStack stack x (TermBinding tm' p1)), MuTildeStep)
-- Evaluating producer variables
computeStep (MkMachineState (TmVar x) p1 cnt p2 stack) = do
    res <- lookupStack p1 stack x
    case res of
        TermBinding tm p -> Right (MkMachineState tm p cnt p2 stack, TermVarStep)
        ContinuationBinding _ _ -> Left "Tried to lookup term but found continuation"
-- Evaluating consumer variables
computeStep (MkMachineState tm p1 (CntVar x) p2 stack) = do
    res <- lookupStack p2 stack x
    case res of
        ContinuationBinding cnt p -> Right (MkMachineState tm p1 cnt p stack, ContinuationVarStep)
        TermBinding _ _ -> Left "Tried to lookup continuation but found term"
computeStep (MkMachineState _ _ CntTop _ _) = Left "Computation finished."

-------------------------------------------------------------------------------
-- Garbage collection / Stack popping
-------------------------------------------------------------------------------

-- | Returns Just if the stack has been restricted, Nothing otherwise.
garbageCollection :: MachineState -> Maybe MachineState
garbageCollection (MkMachineState tm p1 cnt p2 stack) | max p1 p2 < topOfStack stack = Just (MkMachineState tm p1 cnt p2 (restrictStack stack (max p1 p2)))
                                                      | otherwise = Nothing

