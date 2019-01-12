import Commons
import Parser
import Data.List (maximumBy,minimumBy,sort,nub)
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace

type Pos = (Int,Int,Int)
type Bot = (Pos,Int)

inputParser :: Parser Bot
inputParser = do string "pos=<"
                 x <- integer
                 char ','
                 y <- integer
                 char ','
                 z <- integer
                 string ">, r="
                 r <- integer
                 return ((x,y,z), r)

distance :: Pos -> Pos -> Int
distance (x1,y1,z1) (x2,y2,z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

part01 :: [Bot] -> Int
part01 bots = sum [1 | (p',r') <- bots, distance p p' <= r]
    where
        (p,r) = maximumBy (\(_,r) (_,r') -> compare r r') bots


start :: Pos
start = (0,0,0)

part02' bots = run step (x1,y1,z1) (x2,y2,z2)
    where
        ((x1,_,_),_) = minimumBy (\((x,_,_),_) ((x',_,_),_) -> compare x x') bots
        ((x2,_,_),_) = maximumBy (\((x,_,_),_) ((x',_,_),_) -> compare x x') bots
        ((_,y1,_),_) = minimumBy (\((_,y,_),_) ((_,y',_),_) -> compare y y') bots
        ((_,y2,_),_) = maximumBy (\((_,y,_),_) ((_,y',_),_) -> compare y y') bots
        ((_,_,z1),_) = minimumBy (\((_,_,z),_) ((_,_,z'),_) -> compare z z') bots
        ((_,_,z2),_) = maximumBy (\((_,_,z),_) ((_,_,z'),_) -> compare z z') bots
        factor       = 2
        step         = 10000000

        --run :: Int -> Pos -> Pos -> Pos
        run step from' to' | trace ("run -> " ++ show step ++ " -> " ++ show from' ++ " -> " ++ show to') False = undefined
        run step (x1,y1,z1) (x2,y2,z2)
            | step <= 1 = ((x',y',z'),n,d)
            | otherwise = run (step `div` factor) from' to'
            where
                list             = [(x,y,z) | x <- [x1,(x1+step)..x2], y <- [y1,(y1+step)..y2], z <- [z1,(z1+step)..z2]]
                ((x',y',z'),n,d) = foldl findBest (start, 0, maxBound :: Int) list

                margin = step * factor
                from'  = (x'-margin, y'-margin, z'-margin)
                to'    = (x'+margin, y'+margin, z'+margin)

                findBest :: (Pos,Int,Int) -> Pos -> (Pos,Int,Int)
                findBest (p,n,d) p'
                    | n' > n             = (p',n',d')
                    | n' == n && d' <= d = (p',n',d')
                    | otherwise          = (p,n,d)
                    where
                        n' = intersectionSize p'
                        d' = distance start p'

                intersectionSize :: Pos -> Int
                intersectionSize p = length $ filter (\b -> p `isInside` b) bots


-- Part 02: ((45737390,19799677,43081734),983,108618801) <-- winner
-- Part 02: ((44623832,20979837,40788016),918,106391685)
-- Part 02: ((43389173,17610161,36004468),983,97003802)
-- Part 02: ((43389173,17610161,36004468),983,97003802)
-- Part 02: ((43389173,17610161,36004468),983,97003802)




-- 120284879

-- 97003802 
-- 97003802



isInside :: Pos -> Bot -> Bool
isInside (x,y,z) ((x',y',z'),r) = d - r < 0 && d + r > 0
    where
        d = abs (x-x') + abs (y-y') + abs (z-z')
-- isInside (x,y,z) ((x',y',z'),r) = abs (x-x') + abs (y-y') + abs (z-z') <= r
-- isInside (x,y,z) ((x',y',z'),r) = distance (x,y,z) (x',y',z') < r
-- isInside (x,y,z) ((x',y',z'),r) = (x-x')^2 + (y-y')^2 + (z-z')^2 < r^2


solution :: IO ()
solution = do putStr "Part 01: "
              input <- map (runParser inputParser) . lines <$> getInput "input_23.txt"
              print $ part01 input
              putStr "Part 02: "
              print $ part02' input
              --mapM_ print (part02 input)

main :: IO ()
main = solution

-- Part 02: 8505705327168674990
-- Part 02: 6011898498614420980
-- Part 02: 13646030329425600

-- r = 50012575

-- Part 02: 373711403 x
-- Part 02: 225195342 x
-- Part 02: 125170192 x