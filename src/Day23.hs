import Commons
import Parser
import Data.List (maximumBy)

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

solution :: IO ()
solution = do putStr "Part 01: "
              input <- map (runParser inputParser) . lines <$> getInput "input_23.txt"
              print $ part01 input

main :: IO ()
main = solution
