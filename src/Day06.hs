import Commons
import Parser
import Data.Maybe (fromJust)
import Data.List 
import Data.Monoid ((<>))

-- ..........
-- .A........
-- ..........
-- ........C.
-- ...D......
-- .....E....
-- B.........
-- ..........
-- ..........
-- ........F.

-- input :: [Pos]
-- input = [(1,1), (1,6), (8,3), (3,4), (5,5), (8,9)]

type Pos = (Int,Int)

distance :: Pos -> Pos -> Int
distance (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)


collect :: (Int,[Pos]) -> (Int,Pos) -> (Int,[Pos])
collect (d,ps) (d',p) | null ps   = (d', [p]) 
                      | d' < d    = (d', [p])
                      | d' == d   = (d, p:ps)
                      | otherwise = (d, ps)

edges :: [Pos] -> (Pos,Pos,Pos,Pos)
edges ps = (p1,p2,p3,p4)
           where
            ys = sortBy (\(x,y) (x',y') -> compare y y' <> compare x x') ps
            xs = sortBy (\(x,y) (x',y') -> compare x x' <> compare y' y) ps
            (p1,p2,p3,p4) = (head ys, last ys, head xs, last xs)

range :: (Pos,Pos,Pos,Pos) -> ((Pos),(Pos))
range ((x1,y1),(x2,y2),(x3,y3),(x4,y4)) = ((x,y), (x',y'))
                                          where
                                            xs = [x1,x2,x3,x4]
                                            ys = [y1,y2,y3,y4]
                                            x  = minimum xs
                                            x' = maximum xs
                                            y  = minimum ys
                                            y' = maximum ys

singleClosest :: (Pos) -> [Pos] -> Maybe Pos
singleClosest p ps = result
                     where
                        ds      = [(distance p p', p') | p' <- ps]
                        (d,ps') = foldl collect (0,[]) ds
                        result  = if length ps' == 1 then Just (head ps') else Nothing


part01 :: [Pos] -> Int
part01 ps = maximum result
            where
                (p1,p2,p3,p4)     = edges ps
                ((x1,y1),(x2,y2)) = range (p1,p2,p3,p4)
                area              = [(x,y) | x <- [(x1+1)..(x2-1)], y <- [(y1+1)..(y2-1)]] 
                candidates        = map fromJust $ filter (/=Nothing) $ [singleClosest p ps | p <- area]
                insiders          = filter (\p -> p /= p1 || p /= p2 || p /= p3 || p /= p4) candidates
                result            = map length (group $ sort insiders)

part02 :: Int -> [Pos] -> Int
part02 n ps = length result
              where
                ((x1,y1),(x2,y2)) = range (edges ps)
                r                 = n `div` length ps
                area              = [(x,y) | x <- [(x1-r)..(x2+r)], y <- [(y1-r)..(y2+r)]]
                inRadius p        = (sum [distance p p' | p' <- ps]) < n
                result            = foldl (\acc p -> if inRadius p then p:acc else acc) [] area


position :: Parser Pos
position = do x <- int
              symbol ","
              space
              y <- int
              return (x,y)

parseInput :: String -> [Pos]
parseInput = map (runParser position) . lines


solution :: IO ()
solution = do putStr "Part 01: "
              input <- parseInput <$> getInput "input_06.txt"      
              print $ part01 input
              putStr "Part 02: "
              print $ part02 10000 input

main :: IO ()
main = solution
