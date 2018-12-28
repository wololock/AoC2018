import Commons
import Parser
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import Debug.Trace
import Data.List (nub)

type Pos = (Int,Int)

data Flow a = Fall a | Fill a
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
        


solve :: Pos -> HashSet Pos -> (HashSet Pos, HashSet Pos)
solve (x,y) clays = run [Fall (x,y)] Set.empty Set.empty
    where
        maxY :: Int
        maxY = foldl (\y (_,y') -> if y' > y then y' else y) 0 clays

        run :: [Flow Pos] -> HashSet Pos -> HashSet Pos -> (HashSet Pos, HashSet Pos)
        -- run fs water trail     | trace ("run -> " ++ show fs ++ " -> " ++ show (Set.size water) ++ " -> " ++ show (Set.size trail)) False = undefined
        run [] water trail     = (water,trail)
        run (f:fs) water trail = case f of
            Fall p -> fall p fs water trail
            Fill p -> fill p fs water trail

        fall :: Pos -> [Flow Pos] -> HashSet Pos -> HashSet Pos -> (HashSet Pos, HashSet Pos)
        fall (x,y) fs water trail = run (fs ++ fs') water trail'
            where
                stream  = takeWhile p [(x,y') | y' <- [y..]]
                p (x,y) = y < maxY && not ((x,y) `Set.member` clays || (x,y) `Set.member` water)
                trail'  = trail `Set.union` Set.fromList stream
                fs'     | null stream                   = []
                        | y' >= maxY                    = []
                        | (x',y'+1) `Set.member` clays  = [Fill (x',y')]
                        | (x',y'+1) `Set.member` water  = [Fill (x',y')]
                        | otherwise                     = []
                        where
                            (x',y') = last stream

        fill :: Pos -> [Flow Pos] -> HashSet Pos -> HashSet Pos -> (HashSet Pos, HashSet Pos)
        fill (x,y) fs water trail = run (nub $ fs ++ fs') water' trail'
            where
                left    = takeWhile p [(x',y) | x' <- [x,(x-1)..]]
                right   = takeWhile p [(x',y) | x' <- [x..]]
                p (x,y) = not ((x,y) `Set.member` clays) && ((x,y+1) `Set.member` clays || (x,y+1) `Set.member` water)
                
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


solution :: IO ()
solution = do putStr "Part 01: "
              clays <- parseInput <$> getInput "input_17.txt"
              let (water,trail) = solve (500,1) clays
              print $ Set.size (water `Set.union` trail)
              putStr "Part 02: "
              print $ Set.size water


main :: IO ()
main = solution