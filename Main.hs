module Main (main) where

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Euler

-- This is still mad fucked up
-- I have no clue how to get a Maybe into this
getProblemInput :: Int -> IO String
getProblemInput n = readInput ("problem-" ++ (show n) ++ ".txt")

readInput :: String -> IO String
readInput fn = do
    fileExists <- doesFileExist fn 
    if fileExists
        then readFile fn
        else return ""

main = do
    args <- getArgs
    case args of
        [problemNr] -> 
            do 
                let problem = (read problemNr)::Int
                input <- getProblemInput problem
                putStrLn ("Results for problem " ++
                          problemNr ++
                          ": " ++ (eulerProblem input problem))
        _ -> putStrLn "Usage: euler [problem number]"
