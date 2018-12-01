module Day01 where

import Commons
import Data.Set (Set)
import qualified Data.Set as Set


strToInt :: String -> Int
strToInt (x:xs) | x == '-'  = read (x:xs) :: Int
                | otherwise = read xs :: Int

parseInput :: String -> [Int]
parseInput = map strToInt . lines

-- Exemplary inputs
ns1 :: ([Int],Int)
ns1 = ([1,-2,3,1], 2)
ns2 :: ([Int],Int)
ns2 = ([1,-1], 0)
ns3 :: ([Int],Int)
ns3 = ([3,3,4,-2,-4], 10)
ns4 :: ([Int],Int)
ns4 = ([-6,3,8,5,-6], 5)
ns5 :: ([Int],Int)
ns5 = ([7,7,-2,-7,-4], 14)

testCase :: ([Int],Int) -> IO ()
testCase xs = do putStr "\nInput: "
                 putStr $ show (fst xs)
                 putStr " Expected: "
                 print (snd xs)
                 putStr "Result: "
                 let result = test (cycle (fst xs)) [] 0
                 if result == snd xs then
                    putStrLn "PASSED!"
                 else
                    putStrLn ("FAILED! (expected " ++ show (snd xs) ++ " but was " ++ show result ++ ")")
                 return ()

runTests :: IO ()
runTests = do testCase ns1
              testCase ns2
              testCase ns3
              testCase ns4
              testCase ns5

test :: [Int] -> [Int] -> Int -> Int
test (x:xs) cs freq | freq `elem` cs = freq
                    | otherwise      = test xs (freq:cs) (x+freq)      

test' :: [Int] -> Set Int -> Int -> Int
test' (x:xs) set freq | freq `Set.member` set = freq
                      | otherwise             = test' xs (freq `Set.insert` set) (x+freq)
          
solution :: IO ()
solution = do putStrLn "Part 01";
              numbers <- parseInput <$> getInput "input_01.txt";
              print (sum numbers)
              putStrLn "Part 02"
              print (test' (cycle numbers) Set.empty 0)
              

              
              

