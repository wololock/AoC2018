import Commons
import Parser
import Data.Char (toUpper)
import Data.Maybe (fromJust,isNothing)
import Data.Bits ((.&.),(.|.))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Opcode = ADDR | ADDI | MULR | MULI | BANR | BANI | BORR | BORI | SETR | SETI | GTIR | GTRI | GTRR | EQIR | EQRI | EQRR
              deriving (Eq,Ord,Show,Read)

type Instruction = (Opcode,Int,Int,Int)
type Registry = (Int,Int,Int,Int,Int,Int)

ipParser :: Parser Int
ipParser = do string "#ip"
              space
              integer              

instructionParser :: Parser Instruction
instructionParser = do op <- identifier
                       space
                       a <- integer
                       space
                       b <- integer
                       space 
                       c <- integer
                       return ((read $ map toUpper op) :: Opcode, a, b, c)

get :: Registry -> Int -> Int
get (x,_,_,_,_,_) 0 = x
get (_,x,_,_,_,_) 1 = x
get (_,_,x,_,_,_) 2 = x
get (_,_,_,x,_,_) 3 = x
get (_,_,_,_,x,_) 4 = x
get (_,_,_,_,_,x) 5 = x

set :: Registry -> Int -> Int -> Registry
set (a,b,c,d,e,f) n x = case n of
    0 -> (x,b,c,d,e,f)
    1 -> (a,x,c,d,e,f)
    2 -> (a,b,x,d,e,f)
    3 -> (a,b,c,x,e,f)
    4 -> (a,b,c,d,x,f)
    5 -> (a,b,c,d,e,x)


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


execInstructions :: Int -> Map Int Instruction -> Int -> Registry -> (Int,Registry)
execInstructions ip instructions limit reg = run reg 0
    where
        run :: Registry -> Int -> (Int,Registry)
        run reg n
            | n == limit            = (n, reg)
            | isNothing instruction = (n, reg)
            | otherwise             = run reg'' (n+1)
            where
                instruction = Map.lookup (get reg ip) instructions
                reg'        = exec reg (fromJust instruction)
                inum        = 1 + get reg' ip
                reg''       = set reg' ip inum

part01 :: Int -> Map Int Instruction -> Int
part01 ip instructions = result
    where
        (_,(result,_,_,_,_,_)) = execInstructions ip instructions (-1) (0,0,0,0,0,0)

-- Program jumps into loop 10551282 * 10551282 size and it sums
-- divisors of number 10551282. The following implemention simplfies 
-- the problem to:
-- a. finding 10551282 number
-- b. calculating sum of divisors of that number
part02 :: Int -> Map Int Instruction -> Int
part02 ip instructions = result
    where
        (_,(_,_,n,_,_,_)) = execInstructions ip instructions 32 (1,0,0,0,0,0)
        result = foldl (\acc i -> if n `mod` i == 0 then acc + i else acc) 0 [1..n+1]

        

solution :: IO ()
solution = do putStr "Part 01: "
              input <- lines <$> getInput "input_19.txt"
              let ip = runParser ipParser (head input)
              let instructions = Map.fromList $ zip [0..] (map (runParser instructionParser) $ tail input)
              print $ part01 ip instructions
              putStr "Part 02: "
              print $ part02 ip instructions

main :: IO ()
main = solution
