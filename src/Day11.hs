import Commons
import Data.List (maximumBy)
import Data.Array

type Pos = (Int,Int)

hundredDigit :: Int -> Int
hundredDigit n | n < 1000  = n `div` 100
               | otherwise = read [head digits] :: Int
                             where
                                n'     = show (n `mod` 1000)
                                digits = replicate (3 - length n') '0' ++ n'

powerLevel :: Pos -> Int -> Int
powerLevel (x,y) serial = hundredDigit (((rackId * y) + serial) * rackId) - 5
                          where
                             rackId = x + 10  
                          

pos2square :: Pos -> Int -> [Pos]                             
pos2square (x,y) s = [(x',y') | x' <- [x..(x+s-1)], y' <- [y..(y+s-1)]]


part01 :: Int -> [Int] -> (Int, (Int,Pos))
part01 serial xs = maximumBy (\(_,(p,_)) (_,(p',_)) -> compare p p') [(s, maximum [(sum $ map (\p -> grid ! p) $ pos2square (x,y) s, (x,y)) | x <- [1..(300-s+1)], y <- [1..(300-s+1)]]) | s <- xs]
                   where
                     grid = array ((1,1),(300,300)) [((x,y), powerLevel (x,y) serial) | x <- [1..300], y <- [1..300]]
                   

solution :: IO ()
solution = do putStr "Part 01: "
              let serial = 2694
              let (s,(p,(x,y))) = part01 serial [3]
              print $ show x ++ "," ++ show y
              putStr "Part 02: "
              let (s',(p',(x',y'))) = part01 serial [3..24]
              print $ show x' ++ "," ++ show y' ++ "," ++ show s'

main :: IO ()
main = solution
