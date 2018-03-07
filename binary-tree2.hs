import Data.Maybe

data Tree a =   Branch { val :: a, ls :: Maybe (Tree a), rs :: Maybe (Tree a) }
              | Leaf   { val :: a } deriving (Show, Eq)

newTree :: [a] -> Maybe (Tree a)
newTree [] = Nothing
newTree l
    | min == max = Just $ Leaf (l !! min)
    | otherwise  = Just $ Branch (l !! mid) ls rs
  where min     = 0
        max     = length l - 1
        mid     = ceiling $ fromIntegral max / 2 :: Int
        rng s e = drop s . take (e + 1)
        ls      = newTree $ rng min (mid - 1) l
        rs      = newTree $ rng (mid + 1) max l

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
