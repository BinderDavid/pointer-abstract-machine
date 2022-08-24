module Main where

import System.Environment (getArgs)
import System.Exit (exitSuccess)

import AbstractMachine
import Pretty
import Syntax
import Examples (examples)
              
-- Command line argument logic
main :: IO ()
main = do
    args <- getArgs
    run args

run :: [String] -> IO ()
run [s] = case lookup s examples of
            Nothing -> putStrLn "Unknown argument"
            Just cmd ->  runCommand cmd
run _ = putStrLn "Unknown argument"

-- Running an example

runCommand :: Command -> IO ()
runCommand cmd = do
    putStrLn ""
    putStrLn ("Computing command " <> pretty cmd)
    putStrLn ""
    let initialState = embedCommand cmd
    putStrLn "Initial State:"
    putStrLn (pretty initialState)
    runMachine 1 initialState

runMachine :: Int -> MachineState -> IO ()
runMachine n ms = do
    _ <- getLine
    case computeStep ms of
        Left err -> do
            putStrLn err
            exitSuccess
        Right (ms', step) -> do
            putStrLn ("Step " <> show n <> ": " <> pretty step)
            putStrLn (pretty ms')
            runMachine (n + 1) ms'

