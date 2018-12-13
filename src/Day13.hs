import Commons
import Parser
import Data.Array

type Pos = (Int,Int)
type Track = Array Pos Char
type Cart = (Pos,Direction,Int)

data Direction = North | South | West | East
                 deriving (Show,Eq)

char2direction :: Char -> Maybe Direction
char2direction c | c == '>'   = Just East
                 | c == '<'   = Just West
                 | c == 'v'   = Just South
                 | c == '^'   = Just North
                 | otherwise  = Nothing

-- left -> straight -> right
turn :: Direction -> Int -> Direction
turn South n | n `mod` 3 == 0 = East
             | n `mod` 3 == 1 = South
             | otherwise      = West

turn North n | n `mod` 3 == 0 = West
             | n `mod` 3 == 1 = North
             | otherwise      = East

turn East n  | n `mod` 3 == 0 = North
             | n `mod` 3 == 1 = East
             | otherwise      = South

turn West n  | n `mod` 3 == 0 = South
             | n `mod` 3 == 1 = West
             | otherwise      = North


parseCarts :: [String] -> [Cart]
parseCarts = parseLine 0
             where
                parseLine :: Int -> [String] -> [Cart]
                parseLine _ []       = []
                parseLine n (xs:xss) = process ++ parseLine (n+1) xss
                                       where
                                          process :: [Cart]
                                          process = foldl (\acc (m,c) -> case char2direction c of
                                                                          Nothing -> acc
                                                                          Just d  -> ((m,n), d, 0) : acc
                                                    ) [] (zip [0..] xs)

parseTrack :: String -> Track
parseTrack input = create
                   where
                     input' = filter (/='\n') input
                     m      = length (lines input)
                     n      = length input' `div` m                    
                     create = array ((0,0),(n-1,m-1)) [((i `mod` n, i `div` n), get i) | i <- [0..(n*m-1)]]
                     get i
                       | c == '>' || c == '<' = '-'
                       | c == 'v' || c == '^' = '|'
                       | otherwise = c
                       where c = input' !! i


nextPos :: Pos -> Direction -> Pos
nextPos (x,y) North = (x, y-1)
nextPos (x,y) South = (x, y+1)
nextPos (x,y) West  = (x-1, y)
nextPos (x,y) East  = (x+1, y)


move :: Cart -> Track -> Cart
move ((x,y), d, n) t = ((x',y'), d', n')
                       where
                         (x',y') = nextPos (x,y) d
                         c       = t ! (x',y')
                         (d',n') = case c of
                                    '-'  -> (d,n)
                                    '|'  -> (d,n)
                                    '\\' -> case d of
                                             South -> (East,n)
                                             North -> (West,n)
                                             East  -> (South,n)
                                             West  -> (North,n)
                                    '/'  -> case d of
                                             South -> (West,n)
                                             North -> (East,n)
                                             East  -> (North,n)
                                             West  -> (South,n)
                                    '+'  -> (turn d n, n+1)


detectColisions:: [Cart] -> [Pos]
detectColisions carts = fst (foldl (\(l,r) (p,_,_) -> if p `elem` r then (p:l, r) else (l, p:r)) ([],[]) carts)

part01 :: [Cart] -> Track -> Pos
part01 cs t = tick 0 cs
              where
                tick :: Int -> [Cart] -> Pos
                tick n cs' = if null cols then tick (n+1) cs'' else head cols
                             where
                                cols = detectColisions cs'
                                cs'' = map (`move` t) cs'

  
solution :: IO ()
solution = do putStr "Part 01: "
              input <- getInput "input_13.txt"
              let carts = parseCarts (lines input)
              let track = parseTrack input            
              print $ part01 carts track
