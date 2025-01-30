module Problems.Problem96
    ( problem96
    ) where

import Utils.Sudoku
import Data.Maybe (fromJust)

problem96 :: IO Int
problem96 = do
    puzzles <- readSudokuFile "data/sudoku.txt"
    let solvedPuzzles = map (fromJust . solveSudoku) puzzles
    let topLeftNumbers = map getTopLeftNumber solvedPuzzles
    return $ sum topLeftNumbers
