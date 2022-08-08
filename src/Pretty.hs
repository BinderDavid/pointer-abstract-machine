module Pretty where

import Syntax
import AbstractMachine

-- Prettyprinting Syntax

class Pretty a where
    pretty :: a -> String

instance Pretty Term where
    pretty (TmVar v) = v
    pretty (TmMu v cmd) = "mu " <> v <> "." <> pretty cmd
    pretty (TmLambda x tm) = "\\" <> x <> "." <> pretty tm

instance Pretty Continuation where
    pretty (CntVar v) = v
    pretty (CntMu v cmd) = "~mu " <> v <> "." <> pretty cmd
    pretty (CntCallStack tm cnt) = "(" <> pretty tm <> " . " <> pretty cnt <> ")"
    pretty CntTop = "TOP"
 
instance Pretty Command where
    pretty (Cut tm cnt) = "< " <> pretty tm <> " | " <> pretty cnt <> " >"

-- Prettyprinting Abstract Machines

instance Pretty StackBinding where
    pretty (TermBinding tm pt) = pretty tm <> "{" <> show pt <> "}"
    pretty (ContinuationBinding cnt pt) = pretty cnt <> "{" <> show pt <> "}"

instance Pretty Stack where
    pretty (MkStack stack) = unlines (printStackEntry <$> (zip [0..] stack))
      where
        printStackEntry :: (Int, (Var, StackBinding)) -> String
        printStackEntry (i, (var, bnd)) = show i <> " -> " <> var <> "," <> pretty bnd

instance Pretty MachineState where
    pretty (MkMachineState tm pt1 cnt pt2 stack) =
        unlines [ "------------------------------------------------------------"
                , "Term Closure: " <> pretty tm <> "{" <> show pt1 <> "}"
                , "Continuation Closure: " <> pretty cnt <> "{" <> show pt2 <> "}"
                , "Stack:"
                , pretty stack
                , "------------------------------------------------------------"
                ]

