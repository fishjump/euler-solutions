module Main where

import Data.Function ((&))
import qualified Data.Set as Set

-- Largest palindrome product

-- A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

-- Find the largest palindrome made from the product of two 3-digit numbers.

digits :: Integral t => t -> [t]
digits x
  | x < 10 = [x]
  | otherwise = digits quo ++ [rem]
  where
    quo = x `div` 10
    rem = x `mod` 10

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome x
  | h == l = isPalindrome b
  | otherwise = False
  where
    h = head x
    l = last x
    b = x & take (length x - 1) & drop 1

productsInReversedOrd :: (Num a, Ord a, Enum a) => a -> a -> [a]
productsInReversedOrd min max = [x * y | x <- range, y <- range] & Set.fromList & Set.toDescList
  where
    range = [min .. max]

largestPalindromeProduct :: Integral a => [a] -> Maybe a
largestPalindromeProduct [] = Nothing
largestPalindromeProduct (x : xs)
  | isPalindrome $ digits x = Just x
  | otherwise = largestPalindromeProduct xs

main :: IO ()
main = print $ productsInReversedOrd 100 999 & largestPalindromeProduct
