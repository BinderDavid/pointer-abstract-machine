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
-- = mu alpha. < (\x.x) | (\lambda x.x) . alpha
ex2 :: Command
ex2 = Cut (TmMu "alpha" (Cut idTm (CntCallStack idTm (CntVar "alpha")))) CntTop


-- Command line argument logic

main :: IO ()
main = do
    args <- getArgs
    run args

run :: [String] -> IO ()
run ["ex1"] = putStrLn ("\nComputing command " <> pretty ex1 <> "\n") >> runCommand (embedCommand ex1)
run ["ex2"] = putStrLn ("\nComputing command " <> pretty ex2 <> "\n") >> runCommand (embedCommand ex2)
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

