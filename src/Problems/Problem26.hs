module Problems.Problem26
    ( problem26
    ) where

import Data.List (maximumBy)
import Data.Ord (comparing)

-- | Find the length of the recurring cycle for 1/n
findCycleLength :: Int -> Int
findCycleLength n
    | n == 0    = 0
    | otherwise = go n' 1 []
  where
    -- Remove factors of 2 and 5
    n' = removeFactors n
    removeFactors x
        | x `mod` 2 == 0 = removeFactors (x `div` 2)
        | x `mod` 5 == 0 = removeFactors (x `div` 5)
        | otherwise      = x
    
    go 1 _ _ = 0  -- No recurring cycle
    go d r seen
        | r == 0    = 0  -- No recurring cycle
        | r' `elem` seen = length $ takeWhile (/= r') seen
        | otherwise = go d r' (r':seen)
      where
        r' = (r * 10) `mod` d

problem26 :: Int
problem26 = fst $ maximumBy (comparing snd) 
    [(d, findCycleLength d) | d <- [1..999]]
