module Lib
    ( someFunc
    ) where

someFunc :: IO ()
xor:: Bool -> Bool -> Bool
xor True True= True
xor b1 b2 =False 

factorial :: Int-> Int
--factorial 0=1
--factorial n=n*(factorial(n-1))

factorial n
    | n==0 = 1
--  | n>0 = n * (factorial(n-1))
    | n>0 = n* (factorial(n-1))
    | otherwise = factorial(-n)

--

mygcd :: Int-> Int-> Int
mygcd a 0= a
mygcd a b
    | a>=b =mygcd a (mod a b)
    | otherwise = mygcd b a



--
largestdiv:: Int-> Int
largestdiv n= divsearch n (n-1)

divsearch :: Int -> Int -> Int
divsearch n m
    | (mod n m)== 0 = m
    | otherwise = divsearch n (m-1)


--

intlog :: Int-> Int -> Int
intlog k 1 = 0
intlog k n 
    | n>=k =1+ intlog k (div n k)
    | otherwise=0



someFunc = putStrLn "Hello World!"
