-- DESCRIPTION
-- The function h can be used to compare two logical propositions

import Data.Bool (not)

-- Comparing function
--h :: (((Bool, Bool) -> Bool), ((Bool, Bool) -> Bool)) -> Bool
h (f1, f2) = and $ 
  map (\v -> (f1 v) == (f2 v)) 
      [(True,True), (True, False), (False, True), (False, False)]

-- Example of Usage:

k :: (Bool, Bool) -> Bool
k (True, True)   = False
k (True, False)  = True
k (False, True)  = True
k (False, False) = False

j = \(a, b) -> (a||b) && ((not a) || (not b))

works = h (j, k)
