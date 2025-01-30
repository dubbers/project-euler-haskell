module Utils.MathUtils
    ( isPrime
    , getPrimesUpTo
    , sumOfMultiples
    ) where

import qualified Data.Set as Set

isPrime :: Integer -> Bool
isPrime n
    | n <= 1    = False
    | n <= 3    = True
    | even n    = False
    | otherwise = not $ any (\x -> n `mod` x == 0) [3,5..isqrt n]
  where
    isqrt = floor . sqrt . fromIntegral

getPrimesUpTo :: Integer -> [Integer]
getPrimesUpTo n = sieve [2..n]
  where
    sieve []     = []
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

sumOfMultiples :: Integer -> [Integer] -> Integer
sumOfMultiples limit multiples = sum $ Set.fromList
    [x | m <- multiples, x <- [m,m+m..limit-1]]
