module Day01 where

import Commons

strToInt :: String -> Integer
strToInt (x:xs) | x == '-'  = (read (x:xs) :: Integer)
                | otherwise = (read xs :: Integer)

parseInput :: String -> [Integer]
parseInput = map strToInt . lines

ns1 = [1,-2,3,1]
ns2 = [1,-1]
ns3 = [3,3,4,-2,-4]
ns4 = [-6,3,8,5,-6]
ns5 = [7,7,-2,-7,-4]

test :: [Integer] -> [Integer] -> Integer -> Integer
test (x:xs) cs freq | freq `elem` cs = freq
                    | otherwise      = test xs (freq:cs) (x+freq)
        
          
solution :: IO ()
solution = do putStrLn "Part 01";
              numbers <- parseInput <$> getInput "input_01.txt";
              putStrLn $ show $ sum numbers
              putStrLn "Part 02"
              putStrLn $ show $ test (cycle numbers) [] 0



              
              

