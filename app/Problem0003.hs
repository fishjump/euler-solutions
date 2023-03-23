module Main where

-- Largest prime factor

-- The prime factors of 13195 are 5, 7, 13 and 29.

-- What is the largest prime factor of the number 600851475143 ?

isDivisibleBy :: Integral a => a -> a -> Bool
isDivisibleBy x y = (x `mod` y) == 0

isPrime :: Integral a => a -> Bool
isPrime 1 = True
isPrime 2 = True
isPrime x = not $ any (x `isDivisibleBy`) [2 .. (x `div` 2 + 1)]

greatestPrimeFactor :: Integral t => t -> t
greatestPrimeFactor x = f x 2
  where
    f m n
      | isPrime m = m
      | (m `isDivisibleBy` n) && isPrime n = f (m `div` n) 2
      | otherwise = f m (n + 1)

main :: IO ()
main = print $ greatestPrimeFactor 600851475143
