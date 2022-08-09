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
    pretty (CntCallStack tm cnt) = "(" <> pretty tm <> " · " <> pretty cnt <> ")"
    pretty CntTop = "□"
 
instance Pretty Command where
    pretty (Cut tm cnt) = "< " <> pretty tm <> " | " <> pretty cnt <> " >"

-- Prettyprinting Abstract Machines

instance Pretty StackBinding where
    pretty (TermBinding tm pt) = pretty tm <> "{" <> show pt <> "}"
    pretty (ContinuationBinding cnt pt) = pretty cnt <> "{" <> show pt <> "}"

prettyStack :: Pointer -> Pointer -> Stack -> String
prettyStack pt1 pt2 (MkStack stack) = unlines (printStackEntry <$> reverse (zip [0..] (reverse stack)))
      where
        printStackEntry :: (Int, (Var, StackBinding)) -> String
        printStackEntry (i, (var, bnd)) = "│" <> printTermPt pt1 i <> printContinuationPt pt2 i <> show i <> " ⟼ " <> var <> " : " <> pretty bnd

        printTermPt :: Pointer -> Int -> String
        printTermPt pt i | pt == i = " • "
                         | otherwise = "   "

        printContinuationPt :: Pointer -> Int -> String
        printContinuationPt pt i | pt == i = " • "
                                 | otherwise = "   "


horizontalLine :: String
horizontalLine = "────────────────────────────────────────────────────────────────────────────"


instance Pretty MachineState where
    pretty (MkMachineState tm pt1 cnt pt2 stack) =
        unlines [ "┌" <> horizontalLine 
                , "│ Term: " <> pretty tm <> "{" <> show pt1 <> "}"
                , "│ Cont: " <> pretty cnt <> "{" <> show pt2 <> "}"
                , "├" <> horizontalLine
                , "│ Stack:"
                , if null (unStack stack) then "│" else init (prettyStack pt1 pt2 stack)
                , "└" <> horizontalLine
                ]

instance Pretty ComputeStep where
    pretty LambdaStep = "Evaluated a cut between a lambda expression and a call stack."
    pretty MuStep = "Evaluated a cut between a mu abstraction and a continuation."
    pretty MuTildeStep = "Evaluated a cut between a term and a tilde mu abstraction."
    pretty TermVarStep = "Evaluated a term variable by looking up the value in the stack."
    pretty ContinuationVarStep = "Evaluated a continuation variable by looking up the value in the stack."
