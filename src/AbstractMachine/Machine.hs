module AbstractMachine.Machine where

import Syntax
import AbstractMachine.Stack

-------------------------------------------------------------------------------
-- The Pointer Abstract Machine
-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
-- The machine state
-------------------------------------------------------------------------------

data MachineState = MkMachineState Term Pointer Continuation Pointer Stack

embedCommand :: Command -> MachineState
embedCommand (Cut tm cnt) = MkMachineState tm (-1) cnt (-1) emptyStack

data ComputeStep where
    LambdaStep :: ComputeStep
    MuStep :: ComputeStep
    MuTildeStep :: ComputeStep
    TermVarStep :: ComputeStep
    ContinuationVarStep :: ComputeStep
    GarbageCollectionStep :: ComputeStep

computeStep :: MachineState -> Either String (MachineState, ComputeStep)
-- Popping the top of the stack.
computeStep (MkMachineState tm pt1 cnt pt2 stack) | max pt1 pt2 + 1 < topOfStack stack =
    Right (MkMachineState tm pt1 cnt pt2 (restrictStack stack (max pt1 pt2)), GarbageCollectionStep)
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
