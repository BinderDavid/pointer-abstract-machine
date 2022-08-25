module AbstractMachine.Machine where

import Syntax
import AbstractMachine.Stack

-------------------------------------------------------------------------------
-- Head of machine state
-------------------------------------------------------------------------------

data EvalOrder = CBV | CBN deriving (Eq, Ord, Show)

data MachineHead where
    CutState :: EvalOrder -> Term -> Pointer -> Continuation -> Pointer -> MachineHead


-------------------------------------------------------------------------------
-- The machine state
-------------------------------------------------------------------------------

data MachineState = MkMachineState MachineHead Stack

embedCommand :: Command -> MachineState
embedCommand (Cut tm cnt) = MkMachineState (CutState CBV tm (-1) cnt (-1)) emptyStack

data ComputeStep where
    LambdaStep :: ComputeStep
    MuStep :: ComputeStep
    MuTildeStep :: ComputeStep
    TermVarStep :: ComputeStep
    ContinuationVarStep :: ComputeStep
    GarbageCollectionStep :: ComputeStep

computeStep :: MachineState -> Either String (MachineState, ComputeStep)
-------------------------------------------------------------------------------
-- Popping the top of the stack.
-------------------------------------------------------------------------------
computeStep (MkMachineState hd@(CutState _ _ pt1 _ pt2) stack) | max pt1 pt2 + 1 < topOfStack stack = do
    Right (MkMachineState hd (restrictStack stack (max pt1 pt2)), GarbageCollectionStep)
-------------------------------------------------------------------------------
-- Logical Steps
-------------------------------------------------------------------------------
-- Reducing a cut between a lambda abstraction and a call stack
computeStep (MkMachineState (CutState _ (TmLambda x funbody) _p1 (CntCallStack funarg cont) p2) stack) = do
    let newHead = CutState CBV funbody (topOfStack stack) cont p2
    let newStack = extendStack stack x (TermBinding funarg p2)
    let newState = MkMachineState newHead newStack
    pure (newState, LambdaStep)
-------------------------------------------------------------------------------
-- Mu/Tilde mu steps
-------------------------------------------------------------------------------
-- Reducing mu abstractions.
computeStep (MkMachineState (CutState _ (TmMu x (Cut tm cont)) _p1 cont' p2) stack) = do
    let newHead = CutState CBV tm (topOfStack stack) cont (topOfStack stack)
    let newStack = extendStack stack x (ContinuationBinding cont' p2)
    let newState = MkMachineState newHead newStack
    Right (newState, MuStep)
-- Reducing tilde mu abstractions
computeStep (MkMachineState (CutState _ tm' p1 (CntMu x (Cut tm cnt)) _p2) stack) = do
    let newHead = CutState CBV tm (topOfStack stack) cnt (topOfStack stack)
    let newStack = extendStack stack x (TermBinding tm' p1)
    let newState = MkMachineState newHead newStack
    Right (newState, MuTildeStep)
-------------------------------------------------------------------------------
-- Looking up Variables
-------------------------------------------------------------------------------
-- Evaluating producer variables
computeStep (MkMachineState (CutState _ (TmVar x) p1 cnt p2) stack) = do
    (tm,p) <- lookupTermBinding p1 stack x
    pure (MkMachineState (CutState CBV tm p cnt p2) stack, TermVarStep)
-- Evaluating consumer variables
computeStep (MkMachineState (CutState _ tm p1 (CntVar x) p2) stack) = do
    (cnt,p) <- lookupContinuationBinding p2 stack x
    pure (MkMachineState (CutState CBV tm p1 cnt p) stack, ContinuationVarStep)
-------------------------------------------------------------------------------
-- Finish computation
-------------------------------------------------------------------------------
computeStep (MkMachineState (CutState _ _ _ CntTop _) _) =
    Left "Computation finished."
