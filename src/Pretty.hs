module Pretty where

import Syntax

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
 
instance Pretty Command where
    pretty (Cut tm cnt) = "< " <> pretty tm <> " | " <> pretty cnt <> " >"

-- Prettyprinting Abstract Machines

