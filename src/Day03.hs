module Day03 where

import Commons
import Data.Set (Set)
import qualified Data.Set as Set


type Coord = (Int,Int)
type Rect = (Coord, Coord)

parseInput :: String -> [Rect]
parseInput xs = map (\x -> rect (parseCoords (x !! 0)) (parseWidthLenght (x !! 1))) $ map (\x -> drop 2 (words x)) $ lines xs

-- Ex. "141,223:" --> (141,223)
parseCoords :: String -> Coord
parseCoords xs = (x,y)
                 where
                    parsed = split (/=',') (init xs)
                    x      = read (parsed !! 0) :: Int
                    y      = read (parsed !! 1) :: Int

-- Ex. 7x10 -> (7,10)
parseWidthLenght :: String -> Coord
parseWidthLenght xs = (x,y)
                      where
                        parsed = split (/='x') xs
                        x      = read (parsed !! 0) :: Int
                        y      = read (parsed !! 1) :: Int


rect :: Coord -> Coord -> Rect
rect (x,y) (w,h) | w < 1     = error "Width must be greater or equal 1"
                 | h < 1     = error "Height must be greater or equal 1"
                 | otherwise = ((x,y), (x + w -1, y + h - 1))

overlap :: Rect -> Rect -> Bool
overlap ((x1,y1), (x2,y2)) ((x1',y1'), (x2',y2')) = overlapX && overlapY
                                                    where
                                                        overlapX = (maximum [minimum [x1,x2], minimum [x1',x2']]) - (minimum [maximum [x1,x2], maximum [x1',x2']]) < 1
                                                        overlapY = (maximum [minimum [y1,y2], minimum [y1',y2']]) - (minimum [maximum [y1,y2], maximum [y1',y2']]) < 1

intersection :: Rect -> Rect -> Set Coord
intersection r1 r2 | overlap r1 r2 == False = Set.empty
                   | otherwise              = Set.intersection (rect2Coords r1) (rect2Coords r2)


rect2Coords :: Rect -> Set Coord
rect2Coords ((x1,y1), (x2,y2)) = foldl (\acc xy -> xy `Set.insert` acc) Set.empty [(x,y) | x <- [x1..x2], y <- [y1..y2]]

totalIntersection :: [Rect] -> Set Coord -> Set Coord
totalIntersection [] acc     = acc
totalIntersection (r:rs) acc = totalIntersection rs (foldl (\rs' r' -> (intersection r r') `Set.union` rs') acc rs)

bestClaim :: [Rect] -> Int
bestClaim rs = findBestClaim rs rs 1

findBestClaim :: [Rect] -> [Rect] -> Int -> Int
findBestClaim (r:rs) rs' n | all (\r' -> if r == r' then True else not (overlap r r')) rs' = n
                           | otherwise                                                     = findBestClaim rs rs' (n+1)
                         

solution :: IO ()
solution = do putStr "Part 01: ";
              input <- parseInput <$> getInput "input_03.txt";
              print (length $ totalIntersection input Set.empty)
              putStr "Part 02: ";
              print (bestClaim input)