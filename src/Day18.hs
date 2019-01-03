import Commons
import Parser
import Data.Array
import Data.List (sort, group)
import Data.Maybe (fromJust)

type Pos = (Int,Int)
type Area = Array Pos Char


text2Area :: [String] -> Area
text2Area xss = array ((0,0),(n,n)) [((x,y), get x y) | x <- [0..n], y <- [0..n]]
    where
        n = length xss - 1
        get :: Int -> Int -> Char
        get x y = (xss !! y) !! x


adjacents :: Area -> Pos -> String
adjacents area (x,y) 
    | (x,y) == (0,0) = [area ! (1,0), area ! (1,1), area ! (0,1)]
    | (x,y) == (n,0) = [area ! (n-1,0), area ! (n-1,1), area ! (n,1)]
    | (x,y) == (0,n) = [area ! (0,n-1), area ! (1,n-1), area ! (1,n)]
    | (x,y) == (n,n) = [area ! (n-1,n), area ! (n-1,n-1), area ! (n,n-1)]
    | x == 0         = [area ! (0,y-1), area ! (1,y-1), area ! (1,y), area ! (1,y+1), area ! (0,y+1)]
    | x == n         = [area ! (n,y-1), area ! (n-1,y-1), area ! (n-1,y), area ! (n-1,y+1), area ! (n,y+1)]
    | y == 0         = [area ! (x-1,0), area ! (x-1,1), area ! (x,1), area ! (x+1,1), area ! (x+1,0)]
    | y == n         = [area ! (x-1,n), area ! (x-1,n-1), area ! (x,n-1), area ! (x+1,n-1), area ! (x+1,n)]
    | otherwise      = [area ! (x-1,y-1), area ! (x,y-1), area ! (x+1,y-1), area ! (x-1,y), area ! (x+1,y), area ! (x-1,y+1), area ! (x,y+1), area ! (x+1,y+1)]
    where
        (_, (n,_)) = bounds area


transform :: Char -> String -> Char
transform c cs = case c of
    '.' -> if length (filter (=='|') cs) >= 3 then '|' else '.' 
    '|' -> if length (filter (=='#') cs) >= 3 then '#' else '|'
    '#' -> if ('#' `elem` cs) && ('|' `elem` cs) then '#' else '.'


part01 :: Area -> Int -> Int 
part01 area n = run area 0
    where
        (_,(size,_)) = bounds area
        run area i
            | i == n    = product $ map length $ group $ sort $ filter (/='.') $ elems area
            | otherwise = run area' (i+1)
            where
                area' = array ((0,0),(size,size)) [((x,y), transform (area ! (x,y)) (adjacents area (x,y))) | x <- [0..size], y <- [0..size]]



solution :: IO ()
solution = do putStr "Part 01: "
              input <- text2Area . lines <$> getInput "input_18.txt"
              print $ part01 input 10

main :: IO ()
main = solution
