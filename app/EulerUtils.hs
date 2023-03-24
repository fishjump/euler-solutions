module EulerUtils where

import Data.Function ((&))

isDivisibleBy :: Integral a => a -> a -> Bool
isDivisibleBy x y = (x `mod` y) == 0

isUndivisibleBy :: Integral a => a -> a -> Bool
isUndivisibleBy x y = (x `mod` y) /= 0

isPrime :: Integral a => a -> Bool
isPrime x
  | x <= 1 = False
  | otherwise = [2 .. intSqrt2 x] & all (x `isUndivisibleBy`)
  where
    intSqrt2 = floor . sqrt . fromIntegral

digits :: Integral t => t -> [t]
digits x
  | x < 10 = [x]
  | otherwise = digits quo ++ [rem]
  where
    quo = x `div` 10
    rem = x `mod` 10

slice :: Int -> Int -> [a] -> [a]
slice m n xs = xs & take n & drop m

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome x
  | h == l = isPalindrome b
  | otherwise = False
  where
    h = head x
    l = last x
    b = x & slice 1 (length x - 1)