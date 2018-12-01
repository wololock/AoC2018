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