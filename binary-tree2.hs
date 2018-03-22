import Data.Maybe

data Tree a = Branch { val :: a, ls :: Maybe (Tree a), rs :: Maybe (Tree a) }
            | Leaf   { val :: a } deriving (Show)
              
newTree :: [a] -> Maybe (Tree a)
newTree []  = Nothing
newTree [a] = Just $ Leaf a
newTree l   = Just $ Branch mid left right
  where minIndex = 0
        maxIndex = (length l) - 1
        midIndex = ceiling $ fromIntegral maxIndex / 2 :: Int
        
        mid   = l !! midIndex
        left  = newTree $ take midIndex l
        right = newTree $ drop (midIndex + 1) l

addElement :: (Ord a, Eq a) => (Tree a) -> a -> (Tree a)
addElement lf@(Leaf v) e
    | e == v = lf
    | e > v  = Branch e (Just lf) Nothing
    | e < v  = Branch v (Just $ Leaf e) Nothing
addElement b@(Branch v ls rs) e
    | e < v  = addElement (fromJust ls) e
    | e > v  = addElement (fromJust rs) e
    | e == v = b

removeElement :: (Ord a, Eq a) => (Tree a) -> a -> Maybe (Tree a)
removeElement lf@(Leaf v) e
    | e == v   = Nothing
    | otherwise = Just lf
removeElement b@(Branch v ls rs) e
    | e == v = rs
    | e > v  = case rs of
                Nothing   -> Just b
                otherwise -> removeElement (fromJust rs) e
    | e < v  = case rs of
                Nothing   -> Just b
                otherwise -> removeElement (fromJust ls) e

formatStr :: (Eq a, Show a) => Tree a -> String
formatStr (Leaf v)         = "[" ++ show v ++ "]"
formatStr (Branch v ls rs) = "[" ++ left ++ show v ++ right ++ "]"
  where maybeFormat s = if s /= Nothing then formatStr $ fromJust s else []
        left  = maybeFormat ls
        right = maybeFormat rs
