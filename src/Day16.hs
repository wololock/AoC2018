import Commons
import Parser
import Data.Bits ((.&.),(.|.))
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set


data Opcode = ADDR | ADDI | MULR | MULI | BANR | BANI | BORR | BORI | SETR | SETI | GTIR | GTRI | GTRR | EQIR | EQRI | EQRR
              deriving (Eq,Ord,Show)

type Instruction = (Opcode,Int,Int,Int)
type UnknownInstruct = (Int,Int,Int,Int)
type Registry = (Int,Int,Int,Int)

get :: Registry -> Int -> Int
get (x,_,_,_) 0 = x
get (_,x,_,_) 1 = x
get (_,_,x,_) 2 = x
get (_,_,_,x) 3 = x

set :: Registry -> Int -> Int -> Registry
set (a,b,c,d) n x = case n of
    0 -> (x,b,c,d)
    1 -> (a,x,c,d)
    2 -> (a,b,x,d)
    3 -> (a,b,c,x)


exec :: Registry -> Instruction -> Registry
exec reg (op,a,b,c) = case op of
    ADDR -> set reg c (get reg a + get reg b)
    ADDI -> set reg c (get reg a + b)
    MULR -> set reg c (get reg a * get reg b)
    MULI -> set reg c (get reg a * b)
    BANR -> set reg c (get reg a .&. get reg b)
    BANI -> set reg c (get reg a .&. b)
    BORR -> set reg c (get reg a .|. get reg b)
    BORI -> set reg c (get reg a .|. b)
    SETR -> set reg c (get reg a)
    SETI -> set reg c a
    GTIR -> set reg c (if a > get reg b then 1 else 0)
    GTRI -> set reg c (if get reg a > b then 1 else 0)
    GTRR -> set reg c (if get reg a > get reg b then 1 else 0)
    EQIR -> set reg c (if a == get reg b then 1 else 0)
    EQRI -> set reg c (if get reg a == b then 1 else 0)
    EQRR -> set reg c (if get reg a == get reg b then 1 else 0)

opcodes :: [Opcode]
opcodes = [ADDR, ADDI, MULR, MULI, BANR, BANI, BORR, BORI, SETR, SETI, GTIR, GTRI, GTRR, EQIR, EQRI, EQRR]

resolveUnknownInstruction :: (Registry,UnknownInstruct,Registry) -> [Opcode]
resolveUnknownInstruction (input, (id,a,b,c), output) = [op | op <- opcodes, exec input (op,a,b,c) == output]

parsePart01Input :: Parser (Registry, UnknownInstruct, Registry)
parsePart01Input = do string "Before:"
                      space
                      string "["
                      a <- integer
                      string ","
                      b <- integer
                      string ","
                      c <- integer
                      string ","
                      d <- integer
                      string "]"   
                      takeUntil (/='\n')
                      i1 <- integer
                      space
                      i2 <- integer
                      space 
                      i3 <- integer
                      space
                      i4 <- integer
                      takeUntil (/='\n')
                      string "After:"
                      space
                      string "["
                      a' <- integer
                      string ","
                      b' <- integer
                      string ","
                      c' <- integer
                      string ","
                      d' <- integer
                      string "]"
                      return ((a,b,c,d), (i1,i2,i3,i4), (a',b',c',d'))

parsePart02Input :: Parser UnknownInstruct
parsePart02Input = do op <- integer
                      space
                      a <- integer
                      space
                      b <- integer
                      space 
                      c <- integer
                      takeUntil (/='\n')
                      return (op,a,b,c)


part01 :: String -> Int
part01 input = length $ filter (>=3) $ map (length . resolveUnknownInstruction) unknows
    where
        unknows = map (runParser parsePart01Input) (splitOn "\n\n" input)

part02 :: String -> String -> Int
part02 input01 input02 = get result 0
    where
        unknows :: [(Registry,UnknownInstruct,Registry)]
        unknows = map (runParser parsePart01Input) (splitOn "\n\n" input01)     

        grouped :: Map Int (Set Opcode)
        grouped = foldl (\acc (l,(op,a,b,c),r) -> Map.insertWith Set.union op (Set.fromList $ resolveUnknownInstruction (l,(op,a,b,c),r)) acc) Map.empty unknows

        resolveOpcodes :: Map Int (Set Opcode) -> Map Int (Set Opcode)
        resolveOpcodes m
            | length singles == Map.size m = m
            | otherwise                    = resolveOpcodes m'
            where
                singles = foldl Set.union Set.empty $ Map.elems $ Map.filter (\x -> length x == 1) m
                m'      = Map.map (\ops -> if length ops > 1 then ops `Set.difference` singles else ops) m

        cache :: Map Int Opcode
        cache = Map.map (Set.elemAt 0) $ resolveOpcodes grouped

        program :: [UnknownInstruct]
        program = map (runParser parsePart02Input) (tail $ lines input02)

        result :: Registry
        result = foldl (\reg (op,a,b,c) -> exec reg (cache Map.! op, a, b, c)) (0,0,0,0) program



solution :: IO ()
solution = do putStr "Part 01: "
              input <- splitOn "\n\n\n" <$> getInput "input_16.txt"
              print $ part01 (head input)
              putStr "Part 02: "
              print $ part02 (head input) (input !! 1)

main :: IO ()
main = solution
