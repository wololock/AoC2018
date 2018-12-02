module Day02 where

import Commons
import Data.List

parseInput :: String -> [String]
parseInput = concat . map words . lines


checksum :: [String] -> Int
checksum xss = fst tup * snd tup
               where
                yss = map checkCandidate xss
                tup = foldl (\acc tup' -> (fst acc + fst tup', snd acc + snd tup')) (0,0) yss


checkCandidate :: String -> (Int,Int)
checkCandidate xs = (twos, threes)
                    where
                        ys     = nub $ map length (group (sort xs))
                        twos   = if 2 `elem` ys then 1 else 0
                        threes = if 3 `elem` ys then 1 else 0                     


solution :: IO ()
solution = do putStr "Part 01: ";
              input <- parseInput <$> getInput "input_02.txt";
              print (checksum input)
              putStr "Part 02: "
              print "..."