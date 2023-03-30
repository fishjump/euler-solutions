{-# LANGUAGE NumericUnderscores #-}

module Main where

import EulerUtils (digits)

-- Power digit sum

-- 215 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

-- What is the sum of the digits of the number 21000?

ds :: (Integral t, Integral b) => b -> [t]
ds x = digits (2 ^ x)

powerDigitSum :: Integer -> Integer
powerDigitSum = sum . ds

main :: IO ()
main = print $ powerDigitSum 1_000
