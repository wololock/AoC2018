import Commons
import Parser
import Data.List (minimumBy,maximumBy)
import Control.Monad (when)

type Pos = (Int,Int)

parseInput :: String -> [(Pos,Pos)]
parseInput = map (runParser inputParser) . lines

inputParser :: Parser (Pos,Pos)
inputParser = do string "position=<"
                 x <- integer
                 string ", "
                 y <- integer
                 string "> velocity=<"
                 x' <- integer
                 string ", "
                 y' <- integer
                 string ">"
                 return ((x,y), (x',y'))


cls :: IO ()
cls = putStr "\ESC[2J"

writeAt :: Pos -> String -> IO ()
writeAt p xs = do goto p
                  putStr xs

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]

movePoints :: [(Pos,Pos)] -> [(Pos,Pos)]
movePoints ps = [((x + x', y + y'), (x',y')) | ((x,y), (x',y')) <- ps]

drawPoints :: [(Pos,Pos)] -> Pos -> IO ()
drawPoints ps (x',y') = sequence_ [writeAt (x + x', y + y') "█" | ((x,y),_) <- ps]

pointsInRadius :: Int -> [(Pos,Pos)] -> Bool
pointsInRadius r ps = minX + r >= maxX && minY + r >= maxY
                      where
                         ((minX,_),_) = minimumBy (\((x,_),_) ((x',_),_) -> compare x x') ps
                         ((maxX,_),_) = maximumBy (\((x,_),_) ((x',_),_) -> compare x x') ps
                         ((_,minY),_) = minimumBy (\((_,y),_) ((_,y'),_) -> compare y y') ps
                         ((_,maxY),_) = maximumBy (\((_,y),_) ((_,y'),_) -> compare y y') ps

part01 :: [(Pos,Pos)] -> IO ()
part01 ps = do if pointsInRadius 100 ps 
                   then do cls
                           drawPoints ps (-120, -180)
                           wait 2000000
                   else return ()
               part01 (movePoints ps)

solution :: IO ()
solution = do putStr "Part 01: "
              input <- parseInput <$> getInput "input_10.txt"
              part01 input

main :: IO ()
main = solution
