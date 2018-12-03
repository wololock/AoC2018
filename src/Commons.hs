module Commons where

import Data.Char
import Data.List
import System.Environment
import Text.Printf

getInput :: String -> IO String
getInput name =
  do args <- getArgs
     case args of
       []    -> readFile name
       "-":_ -> getContents
       fn:_  -> readFile fn


strToInt :: String -> Int
strToInt (x:xs) | x == '-'  = read (x:xs) :: Int
                | otherwise = read xs :: Int

split :: Eq a => (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split p s  = before : (split p after)
             where
             	before = takeWhile p s
             	after  = drop (length before + 1) s



                