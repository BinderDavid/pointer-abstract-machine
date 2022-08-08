module Main where

import System.Environment (getArgs)

import AbstractMachine
import Pretty
import Syntax
import System.Exit (exitSuccess)

-- Examples

ex1 :: Command
ex1 = Cut (TmLambda "x" (TmVar "x")) CntTop

-- Command line argument logic

main :: IO ()
main = do
    args <- getArgs
    run args

run :: [String] -> IO ()
run ["ex1"] = runCommand (embedCommand ex1)
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
        Right ms' -> runCommand ms'

