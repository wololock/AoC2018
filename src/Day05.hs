module Day05 where

import Commons
import Data.Char

parseInput :: String -> [String]
parseInput = concatMap words . lines

polarize :: Char -> Char -> Bool
polarize c c' = (c /= c') && (toLower c == toLower c')

hasPolarization :: String -> Bool
hasPolarization []        = False
hasPolarization [_]       = False
hasPolarization (x':x:xs) = polarize x' x || hasPolarization (x:xs)

removePolarization :: String -> String
removePolarization []        = []
removePolarization [x]       = [x]
removePolarization (x':x:xs) | polarize x' x = removePolarization xs
                             | otherwise     = x' : removePolarization (x:xs)

part01 :: String -> String
part01 xs = if hasPolarization xs' then part01 xs' else xs'
            where
              xs' = removePolarization xs

part02 :: String -> Int
part02 xs = minimum [length $ part01 (filter (\c' -> toLower c' /= c) xs) | c <- ['a'..'z']]

solution :: IO ()
solution = do putStr "Part 01: "
              input <- getInput "input_05.txt";
              print (length $ part01 input)
              putStr "Part 02: "
              print (part02 input)
