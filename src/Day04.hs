module Day04 where

import Commons
import Parser
import Data.List
import Data.Char
import Data.Time
import Data.Maybe

type Id = Int
newtype Sleep = Sleep (UTCTime, UTCTime)
                deriving Show

type Guard = (Id, [Sleep])

parseDateTimeStr :: String -> UTCTime
parseDateTimeStr xs = fromJust $ (parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M" xs :: Maybe UTCTime)

duration :: Sleep -> Int
duration (Sleep (from,to)) = abs $ (floor (sleepDuration / 60)) :: Int
                             where
                                sleepDuration = diffUTCTime to from

sleepingMinutes :: Sleep -> [Int]
sleepingMinutes (Sleep (from,to)) = [x `mod` 60 | x <- [start..end]]
                                    where
                                      (TimeOfDay _ start _) = localTimeOfDay $ utcToLocalTime utc from
                                      end                   = start + duration (Sleep (from, to)) - 1



groupByGuard :: [String] -> [[String]]
groupByGuard = groupBy (\s1 s2 -> not $ "begins shift" `isSuffixOf` s2)

guardId :: Parser Int
guardId = do symbol "["
             takeUntil (==']')
             symbol "]"
             space
             string "Guard"
             space
             char '#'
             id <- nat
             space
             string "begins shift"
             return id

timestamp :: Parser UTCTime
timestamp = do symbol "["              
               time <- takeUntil (== ']')        
               symbol "]"     
               takeUntil (=='\n')            
               return $ parseDateTimeStr time

parseRow :: [String] -> Guard
parseRow (x:xs) = (runParser guardId x, map (\[x,y] -> Sleep (x,y)) (chunk 2 $ fmap (runParser timestamp) xs))

parseInput :: String -> [Guard]
parseInput = groupRows . sortBy (\(a,_) (b,_) -> compare a b) . map parseRow . groupByGuard . sort . lines

groupRows :: [Guard] -> [Guard]
groupRows rs = map (\row -> foldl (\(id,ds) (id',ds') -> (id', ds ++ ds')) (0,[]) row) $ groupBy (\(a,_) (b,_) -> a == b) rs


-- Solution Part 1
part01 :: [Guard] -> Int
part01 xs = id * mostFreq
            where
              gs          = map (\(id,ts) -> (id, sort $ concatMap sleepingMinutes ts)) xs
              (id,sleeps) = maximumBy (\(_,a) (_,b) -> compare (length a) (length b)) gs
              mostFreq    = head $ maximumBy (\a b -> compare (length a) (length b)) $ group sleeps


-- Solution Part 2
mostFrequentLength :: Eq a => [[a]] -> Int
mostFrequentLength [] = 0
mostFrequentLength xs = maximum $ (map length) xs

part02 :: [Guard] -> Int
part02 xs = id * mostFreq
            where
              gs          = map (\(id,ts) -> (id, group $ sort $ concatMap sleepingMinutes ts)) xs
              (id,sleeps) = maximumBy (\(_,a) (_,b) -> compare (mostFrequentLength a) (mostFrequentLength b)) gs
              mostFreq    = head $ maximumBy (\a b -> compare (length a) (length b)) sleeps


solution :: IO ()
solution = do putStr "Part 01: "
              guards <- parseInput <$> getInput "input_04.txt"            
              print $ part01 guards
              putStr "Part 02: "
              print $ part02 guards
              
