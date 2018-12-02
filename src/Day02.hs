module Day02 where

import Commons
import Data.List (nub,sort,group,maximumBy)
import Data.Function (on)
import Control.Arrow ((***))

parseInput :: String -> [String]
parseInput = concatMap words . lines


checksum :: [String] -> Int
checksum xss = uncurry (*) tup
               where
                yss = map checkCandidate xss
                tup = foldl (\acc tup' -> (((+) (fst acc) *** (+) (snd acc)) tup')) (0,0) yss


checkCandidate :: String -> (Int,Int)
checkCandidate xs = (twos, threes)
                    where
                        ys     = nub $ map length (group (sort xs))
                        twos   = if 2 `elem` ys then 1 else 0
                        threes = if 3 `elem` ys then 1 else 0                     


commonElems :: Eq a => [a] -> [a] -> [a]
commonElems xs ys = map fst (filter (uncurry (==)) $ zip xs ys)

findCommons :: Eq a => [a] -> [[a]] -> [[a]]
findCommons s = map (commonElems s)

longest :: [[a]] -> [a]
longest = maximumBy (compare `on` length)

part2 :: [String] -> [String]
part2 [x]    = [""]
part2 (x:xs) = longest (findCommons x xs) : part2 xs


solution :: IO ()
solution = do putStr "Part 01: ";
              input <- parseInput <$> getInput "input_02.txt";
              print (checksum input)
              putStr "Part 02: "
              print (longest $ part2 input)
              