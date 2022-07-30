module Lib
    ( someFunc
    ) where

someFunc :: IO ()
xor:: Bool -> Bool -> Bool
xor True True= True
xor b1 b2 =False 

factorial :: Int-> Int
factorial 0=1
--factorial n=n*(factorial(n-1))
factorial n
    | n<0 = factorial(-n)
    | n>0 = n * (factorial(n-1))


someFunc = putStrLn "Hello World!"
