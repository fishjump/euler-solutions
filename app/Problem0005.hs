module Main where

-- Smallest multiple

-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

lcm' :: [Integer] -> Integer
lcm' = foldr lcm 1

main :: IO ()
main = print $ lcm' [1 .. 20]
