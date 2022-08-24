module Assignment2(
    l1 
    
) where

import Data.Char

l1 = map isUpper['a' .. 'z'] 
l2 = zipWith (<) [1..26] [1..]

--f = g (\ x -> x)
--g k [] = k []
--g k (x:xs) = g ((x:) . k  ) xs

f =(l , length l) where 
    l=[32,28.3..1]