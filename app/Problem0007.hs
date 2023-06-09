module Main where

import EulerUtils (isPrime)

-- 10001st prime

-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

-- What is the 10 001st prime number?

nthPrime :: (Integral t1, Num t2, Eq t2) => t1 -> t2 -> t1
nthPrime x i
  | i == 0 && isPrime x = x
  | isPrime x = nthPrime (x + 1) (i - 1)
  | otherwise = nthPrime (x + 1) i

main :: IO ()
main = print $ nthPrime 1 10001
