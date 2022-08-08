module Main where

import System.Environment (getArgs)

import AbstractMachine
import Pretty
import Syntax

-- Examples

ex1 :: Command
ex1 = Cut (TmLambda "x" (TmVar "x")) (CntVar "alpha")

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
    let ms' = computeStep ms
    runCommand ms'

