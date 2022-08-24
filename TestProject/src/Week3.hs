module Week3(
    evennum
)
where
import Data.Char

capt :: Char-> Char
offset= ord 'A'  - ord 'a'
capt ch
    | ('a' <= ch && ch <= 'z') = chr ( ord ch + offset )
    | otherwise = ch

touppr :: String -> String
touppr ""= ""
touppr (c:cs) = (capt c):(touppr cs )

evennum :: [Int] -> [Int]
evennum []=[]
evennum (x:xs) 
    | is_even x = (x:evennum xs)
    | otherwise = evennum xs
    where
        is_even :: Int-> Bool
        is_even x = (mod x 2) == 0

--capwow :: [Char] -> [Char]
--capwow l = map touppr ( filter is_vow l)

--is_vow :: Char -> Char
--is_vow c = (c=='a') || (c=='e') || (c=='i') || (c=='o') || (c=='u')     

divisors_of_n :: Int -> [Int]
divisors_of_n n = [x | x <- [1..n], (mod n x)==0 ]

primes :: Int -> [Int]
primes n = [x | x <- [1..n], (divisors_of_n x == [1,x]) ]

-- pytho :: [Int]
pytho = [(x,y,z)| x <- [1..100], y <- [(x+1)..100], z <-[(x+1)..100],x*x +y*y== z*z ]


combine f v []= v 
combine f v (x:xs) = f x (combine f v xs)

sumlist :: [Int] -> Int
sumlist l = combine (+) 0 l



--mylist = [(i,k-i) | k <- [0,2..], i <- [0..]]
myint=(4,4)
mylist = [(i,k-i) | k <- [0,2..], i <- [0..k]]