module Onlinetest(

    f1
)

where


deleteN :: Int -> [a] -> [a]
deleteN _ []     = []
deleteN i (a:as)
   | i == 0    = as
   | otherwise = a : deleteN (i-1) as


f1 :: [a]-> Int -> [a]
f1 x 0 = x
f1 _ 1 = []
f1 x n = f1aux x n 1 (length x)

f1aux :: [a] -> Int -> Int -> Int -> [a]
f1aux x n c len
    | ((n*c)-c) <= (len-1) = f1aux (deleteN ((n*c)-c) x) n (c+1) len
    | otherwise = x
  


-- f2 :: [Int] -> [[Int]]
-- f2 []= [[]]
-- f2 [0] = [[],[0]]
-- f2 (x:xs) =  (f2aux x) ++ (f2 xs)

-- f2aux :: Int -> [[Int]]
-- f2aux 0 = [[],[0]]
-- f2aux a = [[0..a]]


-- f2 :: [a] -> [[a]]
-- f2 x = foldr f2Help [[]] x
-- f2Help :: a -> [[a]] -> [[a]]
-- f2Help x acc = [x] : acc


f2 :: [a] -> [[a]]
f2 [] = [[]]
f2 (x:xs) =
          []:map (x:) (f2 xs)

f3 :: [Int] -> [[Int]]
f3 [] = [[]]
f3 [0] = [[],[0]]
x3 (x:xs) = f3aux [0..x] [[0..x]] ++ f3 xs

f3aux:: [Int] -> [[Int]] -> [[Int]]
f3aux [] _ = [[]]
f3aux [x] _ =[[],[x]]
f3aux (x:xs) y = [xs] ++ f3aux xs y  
