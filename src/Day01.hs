module Day01 where

import Commons

strToInt :: String -> Integer
strToInt (x:xs) | x == '-'  = (read (x:xs) :: Integer)
                | otherwise = (read xs :: Integer)

parseInput :: String -> [Integer]
parseInput = map strToInt . lines

part01 :: IO ()
part01 = do putStrLn "Part 01";
            numbers <- parseInput <$> getInput "input_01.txt";
            putStrLn $ show $ sum numbers
            return ()