module Euler
    (
        eulerProblem
    ) where

divides :: Integer -> Integer -> Bool
divides a b = (b `mod` a) == 0

pFactors :: Integer -> [Integer]
pFactors x = pFactorsHelper [] x
    where pFactorsHelper xs 1 = xs
          pFactorsHelper xs n = pFactorsHelper (divisor:xs) (n `div` divisor)
            where divisor = head (dropWhile (not . (`divides` n)) [2..n])

rle :: (Eq a) => [a] -> [[a]]
rle all@(x:xs) = run:(rle rest)
    where sRun = span (x==) all
          run = fst sRun
          rest = snd sRun
rle _ = []

rleCount :: (Eq a) => [a] -> [(a, Int)]
rleCount = (map counts) . rle
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
eulerProblem 1 = show ( sum (filter ismult [1..999]))
                 where ismult n = (divides 3 n) || (divides 5 n)
eulerProblem 2 = show (sum (filter even (takeWhile (<4000000) fibs)))
eulerProblem 3 = show (maximum (pFactors 600851475143))
eulerProblem 4 = show (maximum (filter isPalindrome
                    (concat
                        (map (\x -> map (*x) [100..x]) [100..999]))))
eulerProblem _ = "Not done yet!"
