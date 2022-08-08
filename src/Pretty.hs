module Pretty where

import Syntax
import AbstractMachine

-- Prettyprinting Syntax

class Pretty a where
    pretty :: a -> String

instance Pretty Term where
    pretty (TmVar v) = v
    pretty (TmMu v cmd) = "μ " <> v <> "." <> pretty cmd
    pretty (TmLambda x tm) = "λ" <> x <> "." <> pretty tm

instance Pretty Continuation where
    pretty (CntVar v) = v
    pretty (CntMu v cmd) = "~μ " <> v <> "." <> pretty cmd
    pretty (CntCallStack tm cnt) = "(" <> pretty tm <> " . " <> pretty cnt <> ")"
    pretty CntTop = "□"
 
instance Pretty Command where
    pretty (Cut tm cnt) = "< " <> pretty tm <> " | " <> pretty cnt <> " >"

-- Prettyprinting Abstract Machines

instance Pretty StackBinding where
    pretty (TermBinding tm pt) = pretty tm <> "{" <> show pt <> "}"
    pretty (ContinuationBinding cnt pt) = pretty cnt <> "{" <> show pt <> "}"

instance Pretty Stack where
    pretty (MkStack stack) = unlines (printStackEntry <$> reverse (zip [0..] (reverse stack)))
      where
        printStackEntry :: (Int, (Var, StackBinding)) -> String
        printStackEntry (i, (var, bnd)) = show i <> " ⟼ " <> var <> " : " <> pretty bnd

instance Pretty MachineState where
    pretty (MkMachineState tm pt1 cnt pt2 stack) =
        unlines [ "------------------------------------------------------------"
                , "Term Closure: " <> pretty tm <> "{" <> show pt1 <> "}"
                , "Continuation Closure: " <> pretty cnt <> "{" <> show pt2 <> "}"
                , "Stack:"
                , pretty stack
                , "------------------------------------------------------------"
                ]

instance Pretty ComputeStep where
    pretty LambdaStep = "Evaluated a cut between a lambda expression and a call stack."
    pretty MuStep = "Evaluated a cut between a mu abstraction and a continuation."
    pretty MuTildeStep = "Evaluated a cut between a term and a tilde mu abstraction."
    pretty TermVarStep = "Evaluated a term variable by looking up the value in the stack."
    pretty ContinuationVarStep = "Evaluated a continuation variable by looking up the value in the stack."