module Euler
    (
        eulerProblem
    ) where

import Data.List (nub, group, transpose, sort)
import Data.Maybe (mapMaybe)
import Data.Char (ord)

-- The actual functions for answering the problems
-- -- 3/9/2014 - just learned about $
-- -- 3/10/2014 - move IO out of this function

type EulerInput = String

eulerProblem :: EulerInput -> Int -> String
eulerProblem _ 1 = show . sum . filter ismult $ [1..999]
                 where ismult n = (divides 3 n) || (divides 5 n)
eulerProblem _ 2 = show . sum . filter even . takeWhile (<4000000) $ fibs
eulerProblem _ 3 = show . maximum . pFactors $ 600851475143
eulerProblem _ 4 = show . maximum
                      . filter isPalindrome
                      . concat
                      . map (\x -> map (*x) [100..x]) $ [100..999]
eulerProblem _ 5 = show . product . map largestMult . uniqueFactors . product $ [1..n]
                 where n = 20
                       largestMult x = x ^ (floor (logBase (fromInteger x) (fromInteger n)))
eulerProblem _ 6 = show $ ((^2) . sum) xs - (sum . map (^2)) xs
                 where xs = [1..100]
eulerProblem _ 7 = show $ primes !! 10000 -- zero indexed!
eulerProblem text 8 = show . maximum . map digitProd . listToInts . groups 5 $ ((head . lines) text)
eulerProblem _ 9 = show . product . head $ [[a,b,c]| c <- [5..998], a <- [3..c], b <- [3..a], a+b+c == 1000, a^2+b^2==c^2] 
eulerProblem _ 10 = show . sum . takeWhile (< 2000000) $ primes
eulerProblem text 11 = show . allMax . allVecs . parseInput $ text
                       where maxVec = maximum . map product . groups 4
                             maxHoriz = map maxVec
                             uppers a = map (\x -> zipWith (!!) a [x..length a -1]) [0..length a - 1]
                             lowers = uppers . transpose
                             rDiags a = uppers a ++ tail (lowers a)
                             lDiags = rDiags . map reverse 
                             allVecs a = filter (\x -> length x >= 4) $ a ++ transpose a ++ rDiags a ++ lDiags a
                             allMax = maximum . maxHoriz
eulerProblem _ 12 = show . head . dropWhile ((< 500) . snd) $ (zip triangles (map nrDivisors triangles))
eulerProblem text 13 = take 10 . show . sum $ ((map read . lines $ text)::[Integer])
eulerProblem _ 14 = show . snd . maximum $ zip (map collatz [1,3..999999]) [1,3..999999]
eulerProblem _ 15 = show ((factorial 40) `div` (factorial 20) ^ 2)
eulerProblem _ 16 = show . digitSum $ 2 ^ 1000
eulerProblem _ 17 = show . sum . map (length . numAsWord) $ [1..1000]
eulerProblem text 18 = show . maximum . pathLengths . parseInput $ text
                    where pathLengths (p:c:xs) = pathLengths $ sumRow (parentMaxes p) c : xs
                          pathLengths xs = head xs
                          sumRow = zipWith (+)
                          parentMaxes ps = map maximum . groups 2 $ (0:ps ++ [0])
eulerProblem _ 19 = show $ length $ firsts 2 months
                    where months = [(x,y) | y <- [1901..2000], x <- [0..11]]
                          firsts 0 (x:xs) = True : firsts ((nrDays x) `mod` 7) xs
                          firsts start (x:xs) = firsts ((start + nrDays x) `mod` 7) xs
                          firsts _ [] = []
                          nrDays (m,y)
                            | m == 1 && (y `mod` 4) == 0 && ((y `mod` 100) /= 0 || (y `mod` 400) == 0) = 29
                            | otherwise = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] !! m
eulerProblem _ 20 = show . digitSum $ factorial 100
eulerProblem _ 21 = show . sum . map fst . filter (\(a,b) -> a /= b) $ mapMaybe getAmicable [1..10000] 
eulerProblem text 22 = show $ sum . map nameScore $ zip (sort parsed) [1..length parsed]
                       where parsed = (read $ '[':text ++ "]")::[String]
                             nameValue = sum . map charValue
                             charValue c = 1 + ord c - ord 'A'
                             nameScore (name, position) = position * nameValue name
eulerProblem text 25 = show $ length . takeWhile (\x -> nrDigits x < 1000) $ fibs
                       where nrDigits = length . show
eulerProblem text 67 = eulerProblem text 18
eulerProblem _ 95 = show  $ drop 999994 $ zip [1..1000000] (map (sum . init . pDivisors) [1..1000000])
-- degenerate case
eulerProblem _ _ = "Not done yet!"


-- Helper functions

divides :: (Integral a) => a -> a -> Bool
divides a b = (b `mod` a) == 0


pFactors :: Integer -> [Integer]
pFactors 1 = []
pFactors n = divisor : pFactors (n `div` divisor)
             where divisor = head (dropWhile (not . (`divides` n)) [2..n])

pDivisors :: (Integral a) => a -> [a]
pDivisors n = lows ++ (reverse . map (n `div`) $ lows)
              where lows = filter (`divides` n) [1..(floor . sqrt $ fromIntegral n)]

uniqueFactors :: Integer -> [Integer]
uniqueFactors = nub . pFactors

rleCount :: (Eq a) => [a] -> [(a, Int)]
rleCount = (map counts) . group
    where counts all@(x:xs) = (x, length all)

factorCounts = rleCount . pFactors

maxDivisor = floor . sqrt . fromInteger

getAmicable :: Int -> Maybe (Int, Int)
getAmicable n = 
    if n == d dn then
        Just (n, dn)
    else
        Nothing
    where d = sum . init . pDivisors -- only proper divisors
          dn = d n

-- Jesus this is beautiful
-- Blowing my mind, here, Haskell
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

primes :: [Integer]
primes = 2 : filter noDivisors [3..]
    where noDivisors n = (not . or . map (`divides` n)) (takeWhile (<= (maxDivisor n)) primes)

triangles :: [Integer]
triangles = map (\x -> x * (x + 1) `div` 2) [1..]

nrDivisors :: Integer -> Int
nrDivisors = (product . map countPlusOne . factorCounts)
               where countPlusOne = (+1) . snd


isPalindrome :: (Show a) => a -> Bool
isPalindrome s = s' == (reverse s')
    where s' = show s

groups :: Int -> [a] -> [[a]]
groups size all@(x:xs)
    | length all >= size = take size all : groups size xs
    | otherwise = []
groups _ _ = []

-- Length of the Collatz sequence starting at n
collatz :: Int -> Int
collatz 0 = 0
collatz 1 = 1
collatz n
    | odd n = 1 + collatz (3 * n + 1)
    | otherwise = 1 + collatz (n `div` 2)

factorial 1 = 1
factorial n = n * factorial (n - 1)

charToDigit :: Char -> Int
charToDigit c = read [c]::Int

strToDigitList :: String -> [Int]
strToDigitList = map charToDigit

digitSum :: (Show a) => a -> Int
digitSum = sum . strToDigitList . show

digitProd :: (Show a) => a -> Int
digitProd = product . strToDigitList . show

listToInts :: [String] -> [Int]
listToInts = map read

numAsWord :: Int -> String
numAsWord 1000 = "onethousand"
numAsWord 0 = ""
numAsWord n 
    | n < 20 = ["one", "two", "three", "four", "five", "six", "seven", "eight",
              "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
              "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"] !! (n - 1)
    | n >= 20 && n < 100 = ["twenty", "thirty", "forty", "fifty", "sixty",
                            "seventy", "eighty", "ninety"] !! (n `div` 10 - 2) ++ numAsWord (n `mod` 10)
    | otherwise = numAsWord (n `div` 100) ++ "hundred" ++
                    if (n `mod` 100) > 0 then
                        "and" ++ numAsWord (n `mod` 100) else
                        ""
parseInput :: String -> [[Int]]
parseInput = map (map read) . map words . lines
