module Main where

import System.Environment (getArgs)

import AbstractMachine
import Pretty
import Syntax
import System.Exit (exitSuccess)

-- Examples

-- | \x.x
idTm :: Term
idTm = TmLambda "x" (TmVar "x")

-- | \x.x
ex1 :: Command
ex1 = Cut idTm CntTop

-- | (\x.x) (\x.x)
-- = mu α. < (\x.x) | (\lambda x.x) . α
ex2 :: Command
ex2 = Cut (TmMu "α" (Cut idTm (CntCallStack idTm (CntVar "α")))) CntTop

-- < μ α.< λx.x | ~μ y.< y | α > > | ~μ x.< x | □ > >
ex3 :: Command
ex3 = Cut (TmMu "α" (Cut idTm (CntMu "y" (Cut (TmVar "y") (CntVar "α"))))) (CntMu "x" (Cut (TmVar "x") CntTop))

-- < μ α.< λx.x | ~μ y.< y | ~μ z.< z | ~μ w.< w | α > > > > | ~μ x.< x | □ > >
ex4 :: Command
ex4 = Cut (TmMu "α" (Cut idTm (CntMu "y" (Cut (TmVar "y")
                                (CntMu "z" (Cut (TmVar "z")
                                  (CntMu "w" (Cut (TmVar "w") (CntVar "α")))))))))
          (CntMu "x" (Cut (TmVar "x") CntTop))

-- the same as ex4, but all variables have the same name
-- < μ α.< λx.x | ~μ x.< x | ~μ x.< x | ~μ x.< x | α > > > > | ~μ x.< x | □ > >
ex5 :: Command
ex5 = Cut (TmMu "α" (Cut idTm (CntMu "x" (Cut (TmVar "x")
                                (CntMu "x" (Cut (TmVar "x")
                                  (CntMu "x" (Cut (TmVar "x") (CntVar "α")))))))))
          (CntMu "x" (Cut (TmVar "x") CntTop))

-- similar to ex4, but use the outer-most bound prd variable instead of the inner-most
-- < μ α.< λx.x | ~μ y.< y | ~μ z.< z | ~μ w.< y | α > > > > | ~μ x.< x | □ > >
ex6 :: Command
ex6 = Cut (TmMu "α" (Cut idTm (CntMu "y" (Cut (TmVar "y")
                                (CntMu "z" (Cut (TmVar "z")
                                  (CntMu "w" (Cut (TmVar "y") (CntVar "α")))))))))
          (CntMu "x" (Cut (TmVar "x") CntTop))

examples :: [(String,Command)]
examples =  [ ("ex1", ex1)
            , ("ex2", ex2)
            , ("ex3", ex3)
            , ("ex4", ex4)
            , ("ex5", ex5)
            , ("ex6", ex6)
            ]
              
-- Command line argument logic
main :: IO ()
main = do
    args <- getArgs
    run args

run :: [String] -> IO ()
run [s] = case lookup s examples of
            Nothing -> putStrLn "Unknown argument"
            Just ex ->  putStrLn ("\nComputing command " <> pretty ex <> "\n") >> runCommand (embedCommand ex)
run _ = putStrLn "Unknown argument"

-- Running an example

runCommand :: MachineState -> IO ()
runCommand ms = do
    putStrLn (pretty ms)
    _ <- getLine
    case computeStep ms of
        Left err -> do
            putStrLn err
            exitSuccess
        Right (ms', step) -> do
            putStrLn (pretty step)
            runCommand ms'

