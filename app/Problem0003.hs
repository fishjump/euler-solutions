module Main where

import Data.Function ((&))
import EulerUtils (isDivisibleBy, isPrime)

-- Largest prime factor

-- The prime factors of 13195 are 5, 7, 13 and 29.

-- What is the largest prime factor of the number 600851475143 ?

greatestPrimeFactor :: Integral t => t -> t
greatestPrimeFactor x = f x 2
  where
    f m n
      | isPrime m = m
      | (m `isDivisibleBy` n) && isPrime n = f (m `div` n) 2
      | otherwise = f m (n + 1)

main :: IO ()
main = print $ greatestPrimeFactor 600851475143
