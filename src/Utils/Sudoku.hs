module Utils.Sudoku
    ( Grid
    , Sudoku(..)
    , readSudokuFile
    , solveSudoku
    ) where

import Data.Array
import Data.Char (digitToInt)
import Data.List (find)
import Data.Maybe (isJust, fromJust)
import Control.Monad (guard)

type Grid = Array (Int, Int) Int

data Sudoku = Sudoku { grid :: Grid }

-- | Create an empty 9x9 grid
emptyGrid :: Grid
emptyGrid = array ((0,0), (8,8)) [((i,j), 0) | i <- [0..8], j <- [0..8]]

-- | Read a Sudoku puzzle from a string
readSudoku :: String -> Sudoku
readSudoku str = Sudoku $ array ((0,0), (8,8))
    [((i,j), digitToInt (lines str !! i !! j)) | i <- [0..8], j <- [0..8]]

-- | Read multiple Sudoku puzzles from a file
readSudokuFile :: FilePath -> IO [Sudoku]
readSudokuFile path = do
    content <- lines <$> readFile path
    return $ parsePuzzles content
  where
    parsePuzzles [] = []
    parsePuzzles (_:xs) = readSudoku (unlines $ take 9 xs) : parsePuzzles (drop 9 xs)

-- | Check if a number is valid in a given position
isValid :: Grid -> (Int, Int) -> Int -> Bool
isValid g (row, col) num = and
    [ notInRow
    , notInCol
    , notInBox
    ]
  where
    notInRow = num `notElem` [g ! (row,j) | j <- [0..8]]
    notInCol = num `notElem` [g ! (i,col) | i <- [0..8]]
    notInBox = num `notElem` 
        [g ! (i,j) | i <- [r0..r0+2], j <- [c0..c0+2]]
    r0 = 3 * (row `div` 3)
    c0 = 3 * (col `div` 3)

-- | Find an empty cell in the grid
findEmpty :: Grid -> Maybe (Int, Int)
findEmpty g = find (\pos -> g ! pos == 0) [(i,j) | i <- [0..8], j <- [0..8]]

-- | Solve a Sudoku puzzle
solveSudoku :: Sudoku -> Maybe Sudoku
solveSudoku (Sudoku g) = case findEmpty g of
    Nothing -> Just $ Sudoku g  -- Puzzle is solved
    Just pos -> do
        num <- find (isValid g pos) [1..9]
        let g' = g // [(pos, num)]
        solveSudoku (Sudoku g')

-- | Get the top-left 3-digit number from a solved Sudoku
getTopLeftNumber :: Sudoku -> Int
getTopLeftNumber (Sudoku g) = 
    100 * (g ! (0,0)) + 10 * (g ! (0,1)) + (g ! (0,2))
