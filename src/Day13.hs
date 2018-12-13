import Commons
import Parser
import Data.Array
import Data.Monoid ((<>))
import Data.List (sortBy)

type Pos = (Int,Int)
type Track = Array Pos Char
type Cart = (Pos,Direction,Turn)

data Direction = North | South | East | West
                 deriving (Eq,Ord,Show)

data Turn = L | S | R
            deriving (Eq,Ord,Show)

char2direction :: Char -> Maybe Direction
char2direction c | c == '>'   = Just East
                 | c == '<'   = Just West
                 | c == 'v'   = Just South
                 | c == '^'   = Just North
                 | otherwise  = Nothing

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
                                                                          Just d  -> ((m,n), d, L) : acc
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


turn :: Direction -> Turn -> (Direction, Turn)
turn North t = case t of
                L -> (West, S)
                S -> (North, R)
                R -> (East, L)
turn South t = case t of 
                L -> (East, S)
                S -> (South, R)
                R -> (West, L)
turn East t = case t of
                L -> (North, S)
                S -> (East, R)
                R -> (South, L)
turn West t = case t of 
                L -> (South, S)
                S -> (West, R)
                R -> (North, L)


nextPos :: Pos -> Direction -> Pos
nextPos (x,y) North = (x, y-1)
nextPos (x,y) South = (x, y+1)
nextPos (x,y) West  = (x-1, y)
nextPos (x,y) East  = (x+1, y)


move :: Cart -> Track -> Cart
move ((x,y), d, r) t = ((x',y'), d', r')
                       where
                         (x',y') = nextPos (x,y) d
                         c       = t ! (x',y')
                         (d',r') = case c of
                                    '-'  -> (d,r)
                                    '|'  -> (d,r)
                                    '\\' -> case d of
                                             South -> (East,r)
                                             North -> (West,r)
                                             East  -> (South,r)
                                             West  -> (North,r)
                                    '/'  -> case d of
                                             South -> (West,r)
                                             North -> (East,r)
                                             East  -> (North,r)
                                             West  -> (South,r)
                                    '+'  -> turn d r


detectColisions:: [Cart] -> [Pos]
detectColisions carts = fst (foldl (\(l,r) (p,_,_) -> if p `elem` r then (p:l, r) else (l, p:r)) ([],[]) carts)

part01 :: Track -> [Cart] -> Pos
part01 t = tick 0
           where
             tick :: Int -> [Cart] -> Pos
             tick n cs = if null cols then tick (n+1) cs' else head cols
                         where
                            (cs',cols) = makeMove (sortBy (\((x,y),_,_) ((x',y'),_,_) -> compare y y' <> compare x x') cs) ([],[])
                            makeMove :: [Cart] -> ([Cart],[Pos])-> ([Cart],[Pos])
                            makeMove [] acc           = acc                    
                            makeMove (c:cs) (ns,cols) = makeMove cs' (ns',cols')
                                                        where
                                                          (p,d,i)     = move c t
                                                          crash       = collision (p,d,i) (cs ++ ns)
                                                          (ns',cols') | crash     = (filter (\(p',_,_) -> p /= p') ns, cols ++ [p])
                                                                      | otherwise = (ns ++ [(p,d,i)], cols)
                                                          cs'         | crash     = filter (\(p',_,_) -> p /= p') cs
                                                                      | otherwise = cs



part02 :: Track -> [Cart] -> Pos
part02 t = tick 0
           where
             tick :: Int -> [Cart] -> Pos
             tick n cs = if length cs' <= 1 then pos (head cs') else tick (n+1) cs'
                         where
                            pos :: Cart -> Pos
                            pos (p,d,n) = p
                            cs'         = makeMove (sortBy (\((x,y),_,_) ((x',y'),_,_) -> compare y y' <> compare x x') cs) []
                            makeMove :: [Cart] -> [Cart]-> [Cart]
                            makeMove [] acc     = acc
                            makeMove (c:cs) acc = makeMove cs' acc'
                                                  where
                                                     (p,d,i) = move c t
                                                     crash   = collision (p,d,i) (cs ++ acc)
                                                     acc'    | crash     = filter (\(p',_,_) -> p /= p') acc
                                                             | otherwise = acc ++ [(p,d,i)]
                                                     cs'     | crash     = filter (\(p',_,_) -> p /= p') cs
                                                             | otherwise = cs

                               
collision :: Cart -> [Cart] -> Bool
collision (p,_,_) cs = any (\(p',_,_) -> p == p') cs


solution :: IO ()
solution = do input <- getInput "input_13.txt"
              let carts = parseCarts (lines input)
              let track = parseTrack input           
              putStr "Part 01: "
              print $ part01 track carts
              putStr "Part 02: "
              print $ part02 track carts
              
main :: IO ()
main = solution
