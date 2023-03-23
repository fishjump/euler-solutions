module Main where

import Data.Function ((&))
import qualified Data.Set as Set
import EulerUtils (digits, isPalindrome)

-- Largest palindrome product

-- A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

-- Find the largest palindrome made from the product of two 3-digit numbers.

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
