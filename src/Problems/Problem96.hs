module Problems.Problem96
    ( problem96
    ) where

import Utils.Sudoku
import Data.Maybe (mapMaybe)

problem96 :: IO Int
problem96 = do
    puzzles <- readSudokuFile "data/sudoku.txt"
    let solvedPuzzles = mapMaybe solveSudoku puzzles
    let topLeftNumbers = map getTopLeftNumber solvedPuzzles
    putStrLn $ "Solved " ++ show (length solvedPuzzles) ++ " out of " ++ show (length puzzles) ++ " puzzles"
    return $ sum topLeftNumbers
