
module Week2(
    mytake
) where

import Data.Char
mytake :: Int -> [Int] -> [Int]

mytake n [] = []
mytake n (x:xs)
    | n<=0 = []
    | otherwise = x:( mytake (n-1) xs)

capt :: Char-> Char
offset= ord 'A'  - ord 'a'
capt ch 
--    | ('a' <= ch && ch <= 'z') = chr ( ord ch + ( ord 'A' - ord 'a') )
    | ('a' <= ch && ch <= 'z') = chr ( ord ch + offset )
    | otherwise = ch

occurss :: Char -> String ->Bool
occurss c ""= False
occurss c (x:xs)
    | c==x = True
    | otherwise = occurss c xs

touppr :: String -> String
touppr ""= ""
touppr (c:cs) = (capt c):(touppr cs )

pos :: Char-> String ->Int
pos c ""=0
pos c (x:xs)
    | c==x = 0
    | otherwise = 1+ (pos c xs)


whitespc :: Char -> Bool
whitespc ' ' = True
whitespc '\t'= True
whitespc '\n'= True
whitespc   _ = False

countword :: String -> Int
countword ""=0
countword (c:cs)
    | whitespc c= 1 + countword cs
    | otherwise = countword cs

wordcaux :: String -> Int
wordcaux [c]=0
wordcaux (c:d:ds) 
    | (whitespc c) && not (whitespc d) = 1+ wordcaux(d:ds)
    | otherwise = wordcaux(d:ds)

wordc :: String -> Int
wordc s= wordcaux(' ':s)

sumpairs :: (Int,Int)-> Int
sumpairs (x,y) = x+y

sumpairslist :: [(Int,Int)] -> Int
sumpairslist ((x,y):xs)= x+y+ sumpairslist xs 

type Point2D = (Float,Float)

distance :: Point2D -> Point2D -> Float
distance (x1,y1) (x2,y2) = sqrt ( sqr  (x2-x1) + sqr (y2-y1) )
    where
    sqr :: Float -> Float
    sqr x = x*x