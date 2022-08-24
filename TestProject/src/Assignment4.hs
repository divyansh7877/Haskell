module Assignment4 (
    f1
)
where


f1 :: [Int]-> [Int]
f1 [] = []
f1 (x:xs)
    | check2pow x = ((2*x):(f1 xs))
    | otherwise = (0: f1 xs)
    where
        check2pow :: Int -> Bool
        check2pow x
            | ceiling (logBase 2 (fromIntegral x)) == floor (logBase 2 (fromIntegral x)) = True
            | otherwise = False

f2 :: [Int] -> [Char]
f2 []= []
f2 l = f2aux (length l) 0  l
    where
        f2aux ::  Int -> Int -> [Int] -> [Char]
        f2aux _ _ [] =[]
        f2aux len ind (x:xs)
            | ind==len  = [] 
            | x>ind =  'a':(f2aux len (ind+1) xs)
            | otherwise = 'b': (f2aux len (ind+1) xs)


f3 :: [Int] -> [Int] 
f3 [] = []
f3 [x] = [x]
f3 (x:y:xs)
    | x==y = f3 (x:xs)
    | otherwise = (x:f3 (y: xs))

-- f4 :: [Int] -> [[Int]]
-- f4 [] = []
-- f4 [x] = [[x]]
-- f4 

