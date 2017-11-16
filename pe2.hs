-- Get the fibonacci sequence until values reach 4 million
-- Get even terms
-- Get their sum

maxi :: (Ord a) => [a] -> a
maxi [x] = x
maxi [] = error "the list can't be empty"
maxi (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maxi xs


values = [1, 2]

fibonacci :: Int -> Int -> Int
fibonacci x y = if (x+y) > (4 * 10^6)
                  then
                    y
                  else do
                    fibonacci y (x + y)
