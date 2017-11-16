import Data.Char

-- Simple length of list as int through recursion implementations in Haskell
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

-- Factorial (like ! in math)
factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n-1)

-- Helper function; turns digits of a number into a list
numToDigitList :: Int -> [Int]
numToDigitList x = [digitToInt n | n <- show x]

-- -- Not needed because 'sum' is a function
-- factorialProductDigitSum :: [Int] -> Int
-- factorialProductDigitSum [] = 0
-- factorialProductDigitSum (x:xs) = x + factorialProductDigitSum xs

-- Add the digits of a number calculated through factorial together
fpds :: Int -> Int
fpds n = sum $ numToDigitList $ factorial n

-- -- Not needed, ^ works as power to in Haskell
-- powerOf :: Int -> Int -> Int
-- powerOf b 1 = b
-- powerOf b e = b * powerOf b (e-1)

-- Add descending numbers to the power of their own value together
-- e.g. 3^3 + 2 ^ 2 + 1 ^ 1
selfPower :: Int -> Int
selfPower 1 = 1
selfPower n = (n ^ n) + selfPower (n - 1)
