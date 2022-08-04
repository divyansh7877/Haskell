module Assignment1(
    func1
) where

func1 :: Int-> Int
func1 n=0

f4 :: Int-> Int
f4 n = g n 0

g :: Int -> Int -> Int
g n a 
    | n==0 = a 
    | otherwise = g q (100*a +r)
        where 
            (q,r)= divMod n 10
