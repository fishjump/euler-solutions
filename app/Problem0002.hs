module Main where

import Data.Function ((&))

-- Even Fibonacci numbers

-- Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:

-- 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

-- By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.

fibonacci :: (Num t, Num a, Ord t) => t -> a
fibonacci 1 = 1
fibonacci 2 = 2
fibonacci x
  | x <= 0 = error "malform input"
  | otherwise = fibonacci (x - 1) + fibonacci (x - 2)

fibSeq :: (Ord t1, Num t2, Num t1, Ord t2) => t2 -> t1 -> [t1]
fibSeq i max
  | fib_i < max = fib_i : fibSeq (i + 1) max
  | otherwise = []
  where
    fib_i = fibonacci i

fibSeqLessThan4M :: [Integer]
fibSeqLessThan4M = fibSeq 1 4000000

main :: IO ()
main = print $ fibSeqLessThan4M & filter even & sum