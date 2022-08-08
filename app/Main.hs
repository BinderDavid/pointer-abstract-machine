module Main where

import System.Environment (getArgs)

import AbstractMachine
import Pretty
import Syntax

-- Examples

ex1 :: Command
ex1 = undefined

run :: [String] -> IO ()
run ["ex1"] = pure ()
run _ = "Unknown argument"

main :: IO ()
main = do
    args <- getArgs
    run args