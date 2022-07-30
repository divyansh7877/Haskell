

fact :: Integer -> Integer
fact 1 = 1
fact n = n * fact (n - 1)

main :: IO ()
main = do
let n = 4
print $ fact n