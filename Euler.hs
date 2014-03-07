module Euler
    (
        eulerProblem
    ) where

import Data.List (nub, group)

divides :: Integer -> Integer -> Bool
divides a b = (b `mod` a) == 0

pFactors :: Integer -> [Integer]
pFactors x = pFactorsHelper [] x
    where pFactorsHelper xs 1 = xs
          pFactorsHelper xs n = pFactorsHelper (divisor:xs) (n `div` divisor)
            where divisor = head (dropWhile (not . (`divides` n)) [2..n])

uniqueFactors :: Integer -> [Integer]
uniqueFactors = nub . pFactors

rleCount :: (Eq a) => [a] -> [(a, Int)]
rleCount = (map counts) . group
    where counts all@(x:xs) = (x, length all)

factorCounts = rleCount . pFactors

-- Jesus this is beautiful
-- Blowing my mind, here, Haskell
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

isPalindrome :: (Show a) => a -> Bool
isPalindrome s = s' == (reverse s')
    where s' = show s

eulerProblem :: Int -> String
eulerProblem 1 = (show . sum . filter ismult) [1..999]
                 where ismult n = (divides 3 n) || (divides 5 n)
eulerProblem 2 = (show . sum . filter even . takeWhile (<4000000)) fibs
eulerProblem 3 = (show . maximum . pFactors) 600851475143
eulerProblem 4 = (show . maximum . filter isPalindrome . concat
                        . map (\x -> map (*x) [100..x])) [100..999]
eulerProblem 5 = (show . product . map largestMult . uniqueFactors . product) [1..n]
                 where n = 20
                       largestMult x = x ^ (floor (logBase (fromInteger x) (fromInteger n)))
eulerProblem 6 = show (((^2) . sum) xs - (sum . map (^2)) xs)
                 where xs = [1..100]
eulerProblem _ = "Not done yet!"
