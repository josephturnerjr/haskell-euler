module Main (main) where

import System.Environment (getArgs)
import Euler

main = do
    args <- getArgs
    case args of
        [problemNr] -> putStrLn ("Results for problem " ++ (head args) ++ ": " ++ (eulerProblem (read problemNr)))
        _ -> putStrLn "Usage: euler [problem number]"
