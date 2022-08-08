{-# LANGUAGE GADTs #-}
module Lib where

import Syntax

-- The pointer abstract machine

type Pointer = Int

data StackBinding where
  TermBinding :: Term -> Pointer -> StackBinding
  ContinuationBinding :: Continuation -> Pointer -> StackBinding

type Stack = [(Var, StackBinding)]

emptyStack :: Stack
emptyStack = []

type MachineState = (Term, Pointer, Continuation, Pointer, Stack)

lookupStack :: Pointer -> Stack -> Var -> StackBinding
lookupStack = undefined

embedCommand :: Command -> MachineState
embedCommand (Cut tm cnt) = (tm, 0, cnt, 0, emptyStack)

computeStep :: MachineState -> MachineState
-- Reducing a cut between a lambda abstraction and a call stack
computeStep (TmLambda x funbody,_p1,CntCallStack funarg cont,p2,stack) =
    (funbody, length stack, cont, p2, (x, TermBinding funarg p2) : stack)
-- Reducing mu abstractions. This implements CBV since they are evaluated before
-- mu-tilde abstractions.
computeStep (TmMu x (Cut tm cont), _p1, cont', p2, stack) =
    (tm, length stack, cont, length stack, (x, ContinuationBinding cont' p2) : stack)
-- Reducing tilde mu abstractions
computeStep (tm', p1, CntMu x (Cut tm cnt), _p2, stack) =
    (tm, length stack, cnt, length stack, (x, TermBinding tm' p1) : stack)
-- Evaluating producer variables
computeStep (TmVar x, p1, cnt, p2, stack) =
    case lookupStack p1 stack x of
        TermBinding tm p -> (tm, p, cnt, p2, stack)
        ContinuationBinding _ _ -> error "Tried to lookup term but found continuation"
-- Evaluating consumer variables
computeStep (tm , p1, CntVar x, p2, stack) =
    case lookupStack p2 stack x of
        ContinuationBinding cnt p -> (tm, p1, cnt, p, stack)
        TermBinding _ _ -> error "Tried to lookup continuation but found term"

garbageCollection :: MachineState -> MachineState
garbageCollection (tm, p1, cnt, p2, stack) =
    (tm, p1, cnt, p2, take (max p1 p2) stack)

