import Commons

parseInput :: String -> [Int]
parseInput = map read . words

type Metadata = [Int]

data Tree = Node Metadata [Tree]

createTree :: [Int] -> (Tree, [Int])                  
createTree (n:x:xs) = (Node (take x xs') childs, drop x xs')
                      where
                        (childs, xs') = createChildren n xs []
                        

createChildren :: Int -> [Int] -> [Tree] -> ([Tree], [Int])
createChildren n xs trees | length trees == n = (trees, xs)
                          | otherwise         = createChildren n xs' trees'
                                                where
                                                    (tree, xs') = createTree xs
                                                    trees'      = tree : trees


sumMetas :: Tree -> Int
sumMetas (Node ms childs) = sum ms + (sum $ map sumMetas childs)


-- Node [10,11,12] []
t1 = fst $ createTree [0,3,10,11,12]
-- Node [2] [Node [99] []]
t2 = fst $ createTree [1,1,0,1,99,2]
-- Node [1,1,2] [Node [10,11,12] [], Node [2] [Node [99] []]]
t3 = fst $ createTree [2,3,0,3,10,11,12,1,1,0,1,99,2,1,1,2]


solution :: IO ()
solution = do putStr "Part 01: "
              (tree,_) <- createTree . parseInput <$> getInput "input_08.txt"
              print $ sumMetas tree

              