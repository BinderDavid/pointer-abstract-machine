module AbstractMachine.Machine where

import Syntax
import AbstractMachine.Stack

-------------------------------------------------------------------------------
-- Head of machine state
-------------------------------------------------------------------------------

data MachineHead where
    CutState :: Term -> Pointer -> Continuation -> Pointer -> MachineHead


-------------------------------------------------------------------------------
-- The machine state
-------------------------------------------------------------------------------

data MachineState = MkMachineState MachineHead Stack

embedCommand :: Command -> MachineState
embedCommand (Cut tm cnt) = MkMachineState (CutState tm (-1) cnt (-1)) emptyStack

data ComputeStep where
    LambdaStep :: ComputeStep
    MuStep :: ComputeStep
    MuTildeStep :: ComputeStep
    TermVarStep :: ComputeStep
    ContinuationVarStep :: ComputeStep
    GarbageCollectionStep :: ComputeStep

computeStep :: MachineState -> Either String (MachineState, ComputeStep)
-- Popping the top of the stack.
computeStep (MkMachineState hd@(CutState _ pt1 _ pt2) stack) | max pt1 pt2 + 1 < topOfStack stack = do
    Right (MkMachineState hd (restrictStack stack (max pt1 pt2)), GarbageCollectionStep)
-- Reducing a cut between a lambda abstraction and a call stack
computeStep (MkMachineState (CutState (TmLambda x funbody) _p1 (CntCallStack funarg cont) p2) stack) =
    Right (MkMachineState (CutState funbody (topOfStack stack) cont p2) (MkStack $ (x, TermBinding funarg p2) : unStack stack), LambdaStep)
-- Reducing mu abstractions. This implements CBV since they are evaluated before
-- mu-tilde abstractions.
computeStep (MkMachineState (CutState (TmMu x (Cut tm cont)) _p1 cont' p2) stack) = do
    let newHead = CutState tm (topOfStack stack) cont (topOfStack stack)
    let newStack = extendStack stack x (ContinuationBinding cont' p2)
    let newState = MkMachineState newHead newStack
    Right (newState, MuStep)
-- Reducing tilde mu abstractions
computeStep (MkMachineState (CutState tm' p1 (CntMu x (Cut tm cnt)) _p2) stack) = do
    let newHead = CutState tm (topOfStack stack) cnt (topOfStack stack)
    let newStack = extendStack stack x (TermBinding tm' p1)
    let newState = MkMachineState newHead newStack
    Right (newState, MuTildeStep)
-- Evaluating producer variables
computeStep (MkMachineState (CutState (TmVar x) p1 cnt p2) stack) = do
    res <- lookupStack p1 stack x
    case res of
        TermBinding tm p -> Right (MkMachineState (CutState tm p cnt p2) stack, TermVarStep)
        ContinuationBinding _ _ -> Left "Tried to lookup term but found continuation"
-- Evaluating consumer variables
computeStep (MkMachineState (CutState tm p1 (CntVar x) p2) stack) = do
    res <- lookupStack p2 stack x
    case res of
        ContinuationBinding cnt p -> Right (MkMachineState (CutState tm p1 cnt p) stack, ContinuationVarStep)
        TermBinding _ _ -> Left "Tried to lookup continuation but found term"
computeStep (MkMachineState (CutState _ _ CntTop _) _) =
    Left "Computation finished."
