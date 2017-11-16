-- Find greatest common divisor
gcd' :: Int -> Int -> Int

gcd' a b
  | a > b = gcd' (a-b) b
  | b > a = gcd' a (b-a)
  | otherwise = a

-- inRange :: Int -> Int -> [Int] -> [Int]
-- inRange a b c
--   | a == b = c
--   -- | a > b = inRange a (b + 1) (c ++ [(b + 1)])
--   -- | b > a = inRange (a + 1) b (c ++ [(a + 1)])

isEven :: Int -> Bool
isEven 0 = True
isEven 1 = False
isEven x = isEven (x - 2)

factorise :: Int -> Int
factorise 1 = 1
factorise x = x * factorise (x - 1)

toThePowerOf :: Int -> Int -> Int
toThePowerOf 1 _ = 0
toThePowerOf _ 0 = 1
toThePowerOf b e = b * toThePowerOf b (e - 1)
