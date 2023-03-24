{-# LANGUAGE NumericUnderscores #-}

module Main where

import Data.Function ((&))
import EulerUtils (isPrime)

-- Summation of primes

-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

-- Find the sum of all the primes below two million.

primesBelow :: Integral t => t -> [t]
primesBelow n = [x | x <- [1 .. n], isPrime x]

main :: IO ()
main = print $ sum $ primesBelow 2_000_000
