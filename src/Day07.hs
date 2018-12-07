import Commons
import Parser
import Data.List

inputParser :: Parser ((Char,Char))
inputParser = do string "Step "
                 a <- item
                 string " must be finished before step "
                 b <- item
                 string " can begin."
                 return (a,b)

parseInput :: String -> [(Char,Char)]
parseInput = map (runParser inputParser) . lines

prepare :: [(Char,Char)] -> [(Char,String)]
prepare input = addMissingSteps reqs letters
                where                
                  sorted  = sortBy (\(a,b) (a',b') -> compare b b') input
                  grouped = groupBy (\(a,b) (a',b') -> b == b') sorted              
                  reqs    = map (foldl (\(_,cs) (a,b) -> (b,a:cs) ) ('_',[])) grouped
                  letters = nub $ sort $ concatMap (\(a,b) -> [a,b]) input                

addMissingSteps :: [(Char,String)] -> String -> [(Char,String)]
addMissingSteps cs xs = sortBy (\(c,_) (c',_) -> compare c c') supplemented
                        where
                           missing      = xs \\ (map fst cs)
                           supplemented = cs ++ (map (\c -> (c,"")) missing)

part01 :: [(Char,Char)] -> String
part01 input = calculateStepsOrder steps steps ""
               where
                  steps  = prepare input

calculateStepsOrder :: [(Char,[Char])] -> [(Char,[Char])] -> String -> String
calculateStepsOrder _ [] acc           = acc
calculateStepsOrder [] ys acc          = calculateStepsOrder ys ys acc
calculateStepsOrder ((c,cs):xs) ys acc | acc `contains` cs = calculateStepsOrder xs (filter (\(c',_) -> c' /= c) ys) (acc ++ [c])
                                       | otherwise         = calculateStepsOrder xs ys acc
contains :: Eq a => [a] -> [a] -> Bool
contains _ []        = True
contains list (x:xs) = x `elem` list && contains list xs

solution :: IO ()
solution = do putStr "Part 01: "
              input <- parseInput <$> getInput "input_07.txt"
              print $ part01 input

