import Commons
import Parser
import Data.List (maximum)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map


parseInput :: String -> (Int,Int)
parseInput = runParser inputParser


inputParser :: Parser (Int,Int)
inputParser = do n <- int
                 string " players; last marble is worth "
                 m <- int
                 string " points"
                 return (n,m)


play :: Int -> Int -> Int
play n x = round 1 (0, Seq.singleton 0) Map.empty
           where
             round :: Int -> (Int, Seq Int) -> IntMap Int -> Int
             round i (cursor, xs) scores | i > x     = maximum scores
                                         | otherwise = if i `mod` 23 == 0 then round next (cursor', xs') scores' else round next (cursor'', xs'') scores
                                                       where
                                                          next     = i + 1
                                                          size     = Seq.length xs
                                                          -- scoring conditions
                                                          cursor'  = (cursor - 7) `mod` size
                                                          (l,r)    = Seq.splitAt cursor' xs
                                                          xs'      = l Seq.>< (Seq.drop 1 r)
                                                          i'       = r `Seq.index` 0
                                                          scores'  = Map.insertWith (+) (i `mod` n) (i + i') scores
                                                          -- nonscoring conditions
                                                          cursor'' = ((cursor + 1) `mod` size) + 1
                                                          (l',r')  = Seq.splitAt cursor'' xs
                                                          xs''     = l' Seq.>< (i Seq.<| r')    

 
solution :: IO ()
solution = do putStr "Part 01: "
              (n,m) <- parseInput <$> getInput "input_09.txt"
              print $ play n m
              putStr "Part 02: "
              print $ play n (m*100)

main :: IO ()
main = solution
            