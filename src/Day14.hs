import Data.Foldable (toList)
import Data.Char (intToDigit)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

part01 :: Seq Int -> (Int,Int) -> Int -> Int
part01 xs (i,j) n 
      | length xs > (n + 10) = read (map intToDigit $ toList (Seq.take 10 (Seq.drop n xs))) :: Int
      | otherwise            = part01 xs' (i',j') n
      where
        s       = Seq.length xs 
        x       = xs `Seq.index` (i `mod` s)
        y       = xs `Seq.index` (j `mod` s)
        xy      = if x+y >= 10 then Seq.singleton 1 Seq.|> ((x+y) `mod` 10) else Seq.singleton (x+y)
        xs'     = xs Seq.>< xy
        (i',j') = ((i + x + 1) `mod` length xs', (j + y + 1) `mod` length xs')


part02 :: Seq Int -> (Int,Int) -> Seq Int -> Int
part02 xs (i,j) ns
      | ns'' == ns = Seq.length left
      | otherwise  = part02 xs' (i',j') ns
      where
        s          = Seq.length xs 
        (left,ns') = Seq.splitAt (s - Seq.length ns - 2) xs
        ns''       = Seq.take (Seq.length ns) ns'
        x          = xs `Seq.index` (i `mod` s)
        y          = xs `Seq.index` (j `mod` s)
        xy         = x + y
        xs'        = if xy >= 10 then xs Seq.|> 1 Seq.|> (xy `mod` 10) else xs Seq.|> xy
        s'         = Seq.length xs'
        (i',j')    = ((i + x + 1) `mod` s', (j + y + 1) `mod` s')


solution :: IO ()
solution = do putStr "Part 01: "
              print $ part01 (Seq.fromList [3,7]) (0,1) 360781
              putStr "Part 02: "
              print $ part02 (Seq.fromList [3,7]) (0,1) (Seq.fromList [3,6,0,7,8,1])
              
main :: IO ()
main = solution
