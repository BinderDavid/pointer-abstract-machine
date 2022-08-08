{-# LANGUAGE GADTs #-}
module Syntax where

type Var = String

data Term where
  TmVar :: Var -> Term
  TmMu :: Var -> Command -> Term
  TmLambda :: Var -> Term -> Term

data Continuation where
  CntVar :: Var -> Continuation
  CntMu :: Var -> Command -> Continuation
  CntCallStack :: Term -> Continuation -> Continuation

data Command where
  Cut :: Term -> Continuation -> Command