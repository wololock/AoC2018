module Day01 where

import Commons
import Data.Set (Set)
import qualified Data.Set as Set

parseInput :: String -> [Int]
parseInput = map strToInt . lines
   
findFirstFreqCycle :: [Int] -> Set Int -> Int -> Int
findFirstFreqCycle (x:xs) set freq | freq `Set.member` set = freq
                                   | otherwise             = findFirstFreqCycle xs (freq `Set.insert` set) (x+freq)
          
solution :: IO ()
solution = do putStr "Part 01: ";
              numbers <- parseInput <$> getInput "input_01.txt";
              print (sum numbers)
              putStr "Part 02: "
              print (findFirstFreqCycle (cycle numbers) Set.empty 0)



-- A few more tests with exemplary inputs
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
                 let result = findFirstFreqCycle (cycle (fst xs)) Set.empty 0
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

