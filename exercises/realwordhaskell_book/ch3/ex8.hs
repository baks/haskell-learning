data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)
              
heightTree :: Tree a -> Integer
heightTree Empty = 0;
heightTree (Node parent left right)  
     |heightLeft > heightRight = heightLeft
     |otherwise = heightRight
     where heightLeft = (heightTree left) + 1
           heightRight = (heightTree right) + 1