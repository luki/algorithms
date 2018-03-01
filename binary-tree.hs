data BinaryTree =   Branch { val :: Int, left :: Maybe BinaryTree, right :: Maybe BinaryTree } 
                  | Leaf { val :: Int } deriving (Show)
                  
getRange :: [a] -> Int -> Int -> [a]
getRange [] _ _ = []
getRange l start end = [l !! i | i <- [start..end]]

getBT :: [Int] -> Maybe BinaryTree
getBT [] = Nothing
getBT l 
    | min == max = Just $ Leaf (l !! min)
    | otherwise = Just $ Branch (l !! mid) (getBT left) (getBT right)
  where min = 0
        max = length l - 1
        mid = ceiling $ fromIntegral max / 2 :: Int
        left = getRange l min (mid - 1)
        right = getRange l (mid + 1) max
