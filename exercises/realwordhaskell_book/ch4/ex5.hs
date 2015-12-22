import Data.Char
import Data.List

loop :: Int -> String -> Int
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  in loop acc' xs

asInt xs = loop 0 xs
    
asInt_fold :: String -> Int
asInt_fold [] = 0
asInt_fold ['-'] = 0
asInt_fold (x:xs) 
    | x == '-' = (-1) * foldl' step 0 xs
    | otherwise = foldl' step 0 (x:xs)
        where step acc x = acc * 10 + digitToInt x