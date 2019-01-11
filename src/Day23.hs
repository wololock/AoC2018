import Commons
import Parser
import Data.List (maximumBy,minimumBy)

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

--part02 :: [Bot] -> Int
part02 bots = x * y * z --(x,y,z)
    where
        ((minX,_,_),r1) = minimumBy (\((x,_,_),_) ((x',_,_),_) -> compare x x') bots
        ((maxX,_,_),r2) = maximumBy (\((x,_,_),_) ((x',_,_),_) -> compare x x') bots
        ((_,minY,_),r3) = minimumBy (\((_,y,_),_) ((_,y',_),_) -> compare y y') bots
        ((_,maxY,_),r4) = maximumBy (\((_,y,_),_) ((_,y',_),_) -> compare y y') bots
        ((_,_,minZ),r5) = minimumBy (\((_,_,z),_) ((_,_,z'),_) -> compare z z') bots
        ((_,_,maxZ),r6) = maximumBy (\((_,_,z),_) ((_,_,z'),_) -> compare z z') bots
        (_,minR)        = minimumBy (\(_,r) (_,r') -> compare r r') bots
        -- x               = length [(minX+r1+minR)..(maxX-r2-minR)]
        -- y               = length [(minY+r3+(minR))..(maxY-r4-(minR `div` 2))]
        -- z               = length [(minZ+r5+minR)..(maxZ-r6-minR)]
        x               = abs ((maxX-minR-r2) - (minX+minR+r1))
        y               = abs ((maxY-minR-r4) - (minY+minR+r3))
        z               = abs ((maxZ-minR-r6) - (minZ+minR+r5))

solution :: IO ()
solution = do putStr "Part 01: "
              input <- map (runParser inputParser) . lines <$> getInput "input_23.txt"
              print $ part01 input
              putStr "Part 02: "
              print $ part02 input

main :: IO ()
main = solution

-- Part 02: 8505705327168674990
-- Part 02: 6011898498614420980
-- r = 50012575

-- Part 02: 373711403 x
-- Part 02: 225195342 x
-- Part 02: 125170192 x