module Main where

-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

-- Find the sum of all the multiples of 3 or 5 below 1000.

isMulipleOf :: Integral a => a -> a -> Bool
isMulipleOf x y = x `mod` y == 0

isMultipleOf3Or5 :: Integral a => a -> Bool
isMultipleOf3Or5 x = (x `isMulipleOf` 3) || (x `isMulipleOf` 5)

multiplesOf3Or5 :: Integral a => a -> [a]
multiplesOf3Or5 x = [x | x <- [1 .. x - 1], isMultipleOf3Or5 x]

sumOfMultiplesOf3Or5 :: Integer -> Integer
sumOfMultiplesOf3Or5 = sum . multiplesOf3Or5

main :: IO ()
main = print $ sumOfMultiplesOf3Or5 1000
