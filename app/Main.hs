module Main (main) where

import System.Environment (getArgs)
import Text.Read (readMaybe)
import qualified Problems.Problem26 as P26
import qualified Problems.Problem96 as P96

main :: IO ()
main = do
    args <- getArgs
    case args of
        [numStr] -> case readMaybe numStr of
            Just num -> solveProblem num
            Nothing -> putStrLn "Invalid problem number"
        _ -> putStrLn "Usage: project-euler-exe <problem-number>"

solveProblem :: Int -> IO ()
solveProblem num = case num of
    26 -> print P26.problem26
    96 -> P96.problem96 >>= print
    _  -> putStrLn $ "Problem " ++ show num ++ " not implemented yet"
