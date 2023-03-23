module Main where

-- Sum square difference

-- The sum of the squares of the first ten natural numbers is,

-- 1^2 + 2^2 + ... + 10^2 = 385

-- The square of the sum of the first ten natural numbers is,

-- (1 + 2 + ... + 10)^2 = 55^2 = 3025

-- Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is

-- 3025 - 385 = 2640

-- Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

range :: [Integer]
range = [1 .. 100]

sumOfSquares :: [Integer] -> Integer
sumOfSquares = foldr (\x y -> x ^ 2 + y) 0

squareOfSum :: (Foldable t, Num a) => t a -> a
squareOfSum xs = sum xs ^ 2

diff :: [Integer] -> Integer
diff range = abs (squareOfSum range - sumOfSquares range)

main :: IO ()
main = print $ diff range
