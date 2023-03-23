module Main where

-- Special Pythagorean triplet

-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
-- a^2 + b^2 = c^2

-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

triplet :: (Enum c, Ord c, Num c) => c -> c -> [(c, c, c)]
triplet min max = [(x, y, z) | x <- [min .. max], y <- [min .. max], let z = max - x - y, x < y, y < z]

specialPythagoreanTriplet :: (Num c, Eq c) => [(c, c, c)] -> Maybe (c, c, c)
specialPythagoreanTriplet [] = Nothing
specialPythagoreanTriplet ((x, y, z) : xs)
  | x ^ 2 + y ^ 2 == z ^ 2 = Just (x, y, z)
  | otherwise = specialPythagoreanTriplet xs

main :: IO ()
main = print $ fmap (\(a, b, c) -> a * b * c) t
  where
    t = specialPythagoreanTriplet $ triplet 1 1000
