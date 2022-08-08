{-# LANGUAGE GADTs #-}
module AbstractMachine where

import Syntax

-- The pointer abstract machine

-- The Stack

type Pointer = Int

data StackBinding where
  TermBinding :: Term -> Pointer -> StackBinding
  ContinuationBinding :: Continuation -> Pointer -> StackBinding

newtype Stack = MkStack { unStack :: [(Var, StackBinding)] }

emptyStack :: Stack
emptyStack = MkStack []

topOfStack :: Stack -> Pointer
topOfStack stack = length (unStack stack)

restrictStack :: Stack -> Pointer -> Stack
restrictStack stack pt = MkStack (take pt $ unStack stack)

extendStack :: Stack -> Var -> StackBinding -> Stack
extendStack stack var bnd = MkStack $ (var, bnd) : unStack stack

-- The Machine

data MachineState = MkMachineState Term Pointer Continuation Pointer Stack

lookupStack :: Pointer -> Stack -> Var -> StackBinding
lookupStack pt stack = lookupStack' (restrictStack stack pt)
  where
    lookupStack' :: Stack -> Var -> StackBinding
    lookupStack' (MkStack []) var = error ("Variable " <> var <> " not in stack.")
    lookupStack' (MkStack ((v,bnd):stack')) v' | v == v' = bnd
                                              | otherwise = lookupStack' (MkStack stack') v'

embedCommand :: Command -> MachineState
embedCommand (Cut tm cnt) = MkMachineState tm 0 cnt 0 emptyStack

computeStep :: MachineState -> Either String MachineState
-- Reducing a cut between a lambda abstraction and a call stack
computeStep (MkMachineState _ _ CntTop _ _) = Left "Computation finished."
computeStep (MkMachineState (TmLambda x funbody) _p1 (CntCallStack funarg cont) p2 stack) =
    Right (MkMachineState funbody (topOfStack stack) cont p2 (MkStack $ (x, TermBinding funarg p2) : unStack stack))
-- Reducing mu abstractions. This implements CBV since they are evaluated before
-- mu-tilde abstractions.
computeStep (MkMachineState (TmMu x (Cut tm cont)) _p1 cont' p2 stack) =
    Right (MkMachineState tm (topOfStack stack) cont (topOfStack stack) (extendStack stack x (ContinuationBinding cont' p2)))
-- Reducing tilde mu abstractions
computeStep (MkMachineState tm' p1 (CntMu x (Cut tm cnt)) _p2 stack) =
    Right (MkMachineState tm (topOfStack stack) cnt (topOfStack stack) (extendStack stack x (TermBinding tm' p1)))
-- Evaluating producer variables
computeStep (MkMachineState (TmVar x) p1 cnt p2 stack) =
    case lookupStack p1 stack x of
        TermBinding tm p -> Right (MkMachineState tm p cnt p2 stack)
        ContinuationBinding _ _ -> Left "Tried to lookup term but found continuation"
-- Evaluating consumer variables
computeStep (MkMachineState tm p1 (CntVar x) p2 stack) =
    case lookupStack p2 stack x of
        ContinuationBinding cnt p -> Right (MkMachineState tm p1 cnt p stack)
        TermBinding _ _ -> Left "Tried to lookup continuation but found term"

garbageCollection :: MachineState -> MachineState
garbageCollection (MkMachineState tm p1 cnt p2 stack) =
    MkMachineState tm p1 cnt p2 (restrictStack stack (max p1 p2))

