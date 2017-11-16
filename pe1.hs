-- The task is to find multiplies of 3 and/or 5 below 1000 and add them together
sumOfThreeAndFiveMultiplies :: Int -> Int
sumOfThreeAndFiveMultiplies maxN = sum [x | x <- [1..maxN], x `mod` 3 == 0 || x `mod` 5 == 0 ]
