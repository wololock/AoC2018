import Commons
import Data.List (sortBy)
import Data.Monoid ((<>))

type Pos = (Int,Int)

hundredDigit :: Int -> Int
hundredDigit n | n < 1000  = n `div` 100
               | otherwise = read ([head digits]) :: Int
                             where
                                n'     = show (n `mod` 1000)
                                digits = (replicate (3 - (length n')) '0') ++ n'

powerLevel :: Pos -> Int -> Int
powerLevel (x,y) serial = (hundredDigit (((rackId * y) + serial) * rackId)) - 5
                          where
                             rackId = x + 10  
                          

pos2square :: Pos -> [Pos]                             
pos2square (x,y) = [(x-1,y-1),(x,y-1),(x+1,y-1),
                    (x-1,y),  (x,y),  (x+1,y),
                    (x-1,y+1),(x,y+1),(x+1,y+1)]


part01 :: Int -> (Int,Pos)
part01 serial = head $ sortBy (\(n,(x,y)) (n',(x',y')) -> compare n' n <> compare x x' <> compare y y') $ [(sum $ map (\p -> powerLevel p serial) (pos2square (x,y)), (x-1,y-1)) | x <- [2..298], y <- [2..298]]

solution :: IO ()
solution = do putStr "Part 01: "
              print $ part01 2694

main :: IO ()
main = solution
