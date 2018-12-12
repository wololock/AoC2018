import Commons
import Parser
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Bits


parseInput :: String -> (IntMap Char, Set String)
parseInput input = (initialState, gen)
                   where
                     lines'       = lines input
                     initialState = runParser initialStateParser (head lines')
                     gen          = Set.fromList $ map fst $ filter (\(_,c) -> c == '#') (map (runParser genParser) (drop 2 lines'))

initialStateParser :: Parser (IntMap Char)
initialStateParser = do string "initial state: "
                        chars <- takeUntil (=='\n')
                        return (Map.fromList (zip [0..] chars))

genParser :: Parser ((String,Char))
genParser = do pattern <- takeUntil (==' ')
               space
               string "=>"
               space
               nextGen <- item
               return (pattern, nextGen)


part01 :: (IntMap Char, Set String) -> Int -> Int
part01 (state, gens) n = sum $ Map.keys $ Map.filter (=='#') result --(, Map.elems result)
                         where
                            result = nextGen state 1
                            nextGen :: IntMap Char -> Int -> IntMap Char
                            nextGen state' i | i > n     = state'
                                             | otherwise = nextGen update (i+1)
                                                           where
                                                             (from,_) = Map.findMin state'
                                                             (to,_)   = Map.findMax state'
                                                             get      = Map.findWithDefault '.'
                                                             plant k  = [get (k-2) state', get (k-1) state', get k state', get (k+1) state', get (k+2) state']
                                                             check xs = if Set.member xs gens then '#' else '.'
                                                             plants   = [check (plant k) | k <- [(from-4)..(to+4)]]
                                                             plants'  = dropWhile (=='.') plants
                                                             margin   = length plants - (length plants')
                                                             update   = Map.fromList $ zip [(from+margin-4)..] (reverse $ dropWhile (=='.') (reverse plants'))


part02 :: (IntMap Char, Set String) -> Int
part02 input = read (left ++ (replicate 7 '0') ++ right) :: Int
               where
                   (left, right) = splitAt 3 (show (part01 input 5000))

solution :: IO ()
solution = do putStr "Part 01: "
              input <- parseInput <$> getInput "input_12.txt"
              print $ part01 input 20
              putStr "Part 02: "
              print $ part02 input

main :: IO ()
main = solution
