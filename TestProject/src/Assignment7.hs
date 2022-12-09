main = do {
            sum <- readInput 0;
            putStrLn $ "The final sum is: " ++ show sum
          }
readInput :: Int -> IO Int;
readInput csum = do {
                            inp <- getLine;
                            if inp == "zero" then
                              readInput csum;
                            else if inp == "one" then
                              readInput (csum + 1);
                            else if inp == "two" then
                              readInput (csum + 2);
                            else if inp == "three" then
                              readInput (csum + 3);
                            else if inp == "four" then
                              readInput (csum + 4);
                            else if inp == "five" then
                              readInput (csum + 5);
                            else if inp == "six" then
                              readInput (csum + 6);
                            else if inp == "seven" then
                              readInput (csum + 7);
                            else if inp == "eight" then
                              readInput (csum + 8);
                            else if inp == "nine" then
                              readInput (csum + 9);
                            else if inp == "sum" then do
                              putStrLn $ "The current sum is: " ++ show csum
                              readInput csum;
                            else return csum
                          }