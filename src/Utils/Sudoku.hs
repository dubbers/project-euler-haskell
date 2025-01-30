module Utils.Sudoku
    ( Grid
    , Sudoku(..)
    , readSudokuFile
    , solveSudoku
    , getTopLeftNumber
    ) where

import Data.Char (digitToInt)
import qualified Data.Vector.Unboxed as V

type Grid = V.Vector Int

data Sudoku = Sudoku { grid :: Grid }
    deriving (Show)

-- | Read a Sudoku puzzle from a string (9 lines of 9 characters)
readSudoku :: String -> Sudoku
readSudoku str = Sudoku $ V.fromList [digitToInt (lines str !! i !! j) | i <- [0..8], j <- [0..8]]

-- | Read multiple Sudoku puzzles from a file
readSudokuFile :: FilePath -> IO [Sudoku]
readSudokuFile path = do
    content <- lines <$> readFile path
    return $ parsePuzzles content
  where
    parsePuzzles [] = []
    parsePuzzles (_:xs) = readSudoku (unlines $ take 9 xs) : parsePuzzles (drop 9 xs)

-- | Check if a value is valid at a position
isValid :: Grid -> Int -> Int -> Bool
isValid g pos val = and [checkRow, checkCol, checkBox]
  where
    row = pos `div` 9
    col = pos `mod` 9
    box_row = 3 * (row `div` 3)
    box_col = 3 * (col `div` 3)
    
    checkRow = val `notElem` [g V.! (row * 9 + j) | j <- [0..8]]
    checkCol = val `notElem` [g V.! (i * 9 + col) | i <- [0..8]]
    checkBox = val `notElem` [g V.! (i * 9 + j) | 
                             i <- [box_row..box_row+2],
                             j <- [box_col..box_col+2]]

-- | Find first empty position with fewest valid values
findBestEmpty :: Grid -> Maybe Int
findBestEmpty g = if null empties then Nothing else Just (minimum empties)
  where
    empties = [i | i <- [0..80], g V.! i == 0]

-- | Solve a Sudoku puzzle
solveSudoku :: Sudoku -> Maybe Sudoku
solveSudoku (Sudoku g) = case findBestEmpty g of
    Nothing -> Just $ Sudoku g
    Just pos -> tryValues [1..9]
      where
        tryValues [] = Nothing
        tryValues (v:vs)
            | isValid g pos v = case solveSudoku (Sudoku $ g V.// [(pos, v)]) of
                                 Just s -> Just s
                                 Nothing -> tryValues vs
            | otherwise = tryValues vs

-- | Get the top-left 3-digit number from a solved Sudoku
getTopLeftNumber :: Sudoku -> Int
getTopLeftNumber (Sudoku g) = 100 * (g V.! 0) + 10 * (g V.! 1) + (g V.! 2)
