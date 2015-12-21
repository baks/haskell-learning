sortTwo :: [Integer] -> [Integer] -> [Integer]
sortTwo [] [] = []
sortTwo [] ys = ys
sortTwo xs ys = [y | y <- ys, y <= head xs] ++ [head xs] ++ sortTwo (tail xs) [y | y <- ys, y > head xs]

sortInts :: [Integer] -> [Integer]
sortInts (x:[]) = x:[]
sortInts (x:y:[]) = case x `compare` y of GT -> y:x:[]
                                          LT -> x:y:[]
                                          EQ -> x:y:[]
sortInts xs = case last left `compare` head right of GT -> sortTwo left right
                                                     LT -> left ++ right
                                                     EQ -> left ++ right
    where left = sortInts(take n xs)
          right = sortInts(drop n xs)
          n = length xs `div` 2 
          
sortTwo' :: [[Integer]] -> [[Integer]] -> [[Integer]]
sortTwo' [] [] = []
sortTwo' [] ys = ys
sortTwo' xs ys = [y | y <- ys, length y <= length headFirst]  ++ [headFirst] ++ sortTwo' (tail xs) [y | y <- ys, length y > length (headFirst)]
     where headFirst = head xs

sortBy :: [[Integer]] -> [[Integer]]
sortBy [] = []
sortBy (x:[]) = x:[]
sortBy (x:y:[]) = case (length x) `compare` (length y) of GT -> y:x:[]
                                                          LT -> x:y:[]
                                                          EQ -> x:y:[]
sortBy xs = case (last left) `compare ` (head right) of GT -> sortTwo' left right
                                                        LT -> left ++ right
                                                        EQ -> left ++ right
     where left = sortBy(take n xs)
           right = sortBy(drop n xs)
           n = length xs `div` 2