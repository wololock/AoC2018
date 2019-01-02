import Commons
import Parser
import Data.List (sort, group)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Pos = (Int,Int)
type Area = Map Pos Char

text2Area :: [String] -> Area
text2Area xss = Map.fromList [((x,y), get x y) | x <- [0..n], y <- [0..n]]
    where
        n = (length xss) - 1
        get :: Int -> Int -> Char
        get x y = (xss !! y) !! x

adjacents :: Area -> Pos -> [Char]
adjacents area (x,y) = Map.elems $ Map.filterWithKey (\k _ -> k `elem` surroundings) area
    where
        surroundings = [(x+x',y+y') | x' <- [-1..1], y' <- [-1..1], (x',y') /= (0,0)]

transform :: Char -> [Char] -> Char
transform c cs = case c of
    '.' -> if length (filter (=='|') cs) >= 3 then '|' else '.' 
    '|' -> if length (filter (=='#') cs) >= 3 then '#' else '|'
    '#' -> if ('#' `elem` cs) && ('|' `elem` cs) then '#' else '.'

--part01 :: Area -> Int -> Int 
part01 area n = run area 0
    where
        --run :: Area -> Int -> Int 
        run area i
            | i == n    = product $ map length $ group $ sort $ filter (/='.') $ Map.elems area
            | otherwise = run area' (i+1)
            where
                area' = Map.fromList [((x,y), transform c (adjacents area (x,y))) | ((x,y), c) <- Map.assocs area]

solution :: IO ()
solution = do putStr "Part 01: "
              input <- text2Area <$> lines <$> getInput "input_18.txt"
              -- print $ input ! (0,-1)
              --print input
              --print $ adjacents input (1,1)
              print $ part01 input 10
              --print $ adjacents input (3,9)


main :: IO ()
main = solution
