import Commons
import Parser
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import Debug.Trace
import Data.Maybe (fromJust)
import Data.List (nub)

type Pos = (Int,Int)
data Mode = FALL | FILL 
            deriving (Eq,Show)

parseInput :: String -> HashSet Pos
parseInput = Set.fromList . concatMap (convertToPos . runParser parser) . lines 

convertToPos :: ((Char,Int),(Char,[Int])) -> [(Int,Int)]
convertToPos ((var1,val1),(var2,val2)) = case var1 of
    'x' -> [(val1,y) | y <- val2]
    _   -> [(x,val1) | x <- val2]

parser :: Parser ((Char,Int),(Char,[Int]))
parser = do var1 <- letter
            char '='
            val1 <- integer
            char ','
            space
            var2 <- letter
            char '='
            from <- integer
            string ".."
            to <- integer
            return ((var1, val1), (var2, [from..to]))
            

safelast :: [a] -> Maybe a
safelast [] = Nothing
safelast xs = Just (last xs)


data Flow a = Fall a | Fill a
              deriving (Eq,Show)


--test :: Pos -> HashSet Pos -> Int
test (x,y) clays = Set.size $ run [Fall (x,y)] Set.empty Set.empty
    where
        maxY :: Int
        maxY = foldl (\y (_,y') -> if y' > y then y' else y) 0 clays

        run :: [Flow Pos] -> HashSet Pos -> HashSet Pos -> HashSet Pos
        -- run fs water trail     | trace ("run -> " ++ show fs ++ " -> " ++ show (Set.size water) ++ " -> " ++ show (Set.size trail)) False = undefined
        run [] water trail     = water `Set.union` trail
        run (f:fs) water trail = case f of
            Fall p -> fall p fs water trail
            Fill p -> fill p fs water trail

        fall :: Pos -> [Flow Pos] -> HashSet Pos -> HashSet Pos -> HashSet Pos
        fall (x,y) fs water trail = run (fs ++ fs') water trail'
            where
                stream  = takeWhile p [(x,y') | y' <- [y..]]
                p (x,y) = y < maxY && not ((x,y) `Set.member` clays || (x,y) `Set.member` water)
                trail'  = trail `Set.union` (Set.fromList stream)
                --water'  = water `Set.union` Set.fromList stream
                fs'     | null stream                   = []
                        | y' >= maxY                    = []
                        | (x',y'+1) `Set.member` clays  = [Fill (x',y')]
                        | (x',y'+1) `Set.member` water  = [Fill (x',y')] -- if y' >= maxY then [] else [Fill (x',y')]
                        | otherwise                     = []
                        where
                            (x',y') = last stream

        fill :: Pos -> [Flow Pos] -> HashSet Pos -> HashSet Pos -> HashSet Pos
        fill (x,y) fs water trail = run (nub $ fs ++ fs') water' trail'
            where
                left    = takeWhile p [(x',y) | x' <- [x,(x-1)..]]
                right   = takeWhile p [(x',y) | x' <- [x..]]
                p (x,y) = not ((x,y) `Set.member` clays) && ((x,y+1) `Set.member` clays || (x,y+1) `Set.member` water)
                --water'  = water `Set.union` Set.fromList (left ++ right)
                
                (lx,ly) = if null left then (x,y) else last left
                (rx,ry) = if null right then (x,y) else last right
                lt      = (lx-1,ly) `Set.member` clays
                rt      = (rx+1,ry) `Set.member` clays

                water'  = if lt && rt then water `Set.union` Set.fromList (left ++ right) else water
                trail'  = if lt && rt then trail else trail `Set.union` Set.fromList (left ++ right)

                fs'     = if lt && rt then [Fill (x,y-1)] else case (lt,rt) of 
                    (True,_) -> [Fall (rx+1,ry)]
                    (_,True) -> [Fall (lx-1,ly)]
                    _        -> [Fall (rx+1,ry), Fall (lx-1,ly)]








part01 :: Pos -> HashSet Pos -> Int
part01 (x,y) clays = Set.size (run (x,y) Set.empty FALL)
    where
        maxY :: Int
        maxY = foldl (\y' (_,y) -> if y > y' then y else y') 0 clays

        fall :: Pos -> HashSet Pos -> (Maybe Pos, HashSet Pos)        
        fall (x,y) water = (safelast stream, water `Set.union` Set.fromList stream)
            where
                stream = takeWhile (\(x,y) -> y <= maxY && not ((x,y) `Set.member` clays) && not ((x,y+1) `Set.member` water)) [(x,y') | y' <- [y..]]

        fill :: Pos -> HashSet Pos -> (Maybe Pos, Maybe Pos, HashSet Pos)
        fill (x,y) water = (nextpos (subtract 1) (safelast left), nextpos (+1) (safelast right), water `Set.union` Set.fromList (left ++ right))
            where
                left            = takeWhile predicate [(x',y) | x' <- [x,(x-1)..]]
                right           = takeWhile predicate [(x',y) | x' <- [x..]]            
                predicate (x,y) = not ((x,y) `Set.member` clays) && ((x,y+1) `Set.member` clays || (x,y+1) `Set.member` water)
                nextpos f p     = case p of
                    Just (x,y) -> if (f x, y) `Set.member` clays then Nothing else Just (f x, y)
                    _          -> Nothing

        run :: Pos -> HashSet Pos -> Mode -> HashSet Pos
        -- run p w m | trace ("run -> " ++ show p ++ " -> " ++ show (Set.size w) ++ " -> " ++ show m) False = undefined
        run (x,y) water FALL 
            | isComplete = water'
            | otherwise  = run (fromJust p) water' FILL
            where
                (p,water') = fall (x,y) water
                isComplete = case p of
                    Just (x',y') -> y' >= maxY
                    _            -> True

            -- | isComplete = water'
            -- | otherwise  = run (x',y') water' FILL
            -- where
            --     ((x',y'), water') = fall (x,y) water
            --     isComplete        = y' >= maxY -- || (x',y'+1) `Set.member` water'

        run (x,y) water FILL
            | isComplete = case (lt,rt) of
                (Just p1, Just p2) -> run p1 (run p2 water' FALL) FALL --run p1 water' FALL `Set.union` run p2 water' FALL
                (Just p1, Nothing) -> run p1 water' FALL
                (Nothing, Just p2) -> run p2 water' FALL
            | otherwise  = run (x,y-1) water' FILL
            where
                (lt, rt, water') = fill (x,y) water
                isComplete       = case (lt,rt) of
                    (Nothing,Nothing) -> False
                    _                 -> True
        

solution :: IO ()
solution = do putStr "Part 01: "
              clays <- parseInput <$> getInput "input_17.txt"
              --print $ part01 (500,1) clays
              -- print clays
              print $ test (500,1) clays


main :: IO ()
main = solution