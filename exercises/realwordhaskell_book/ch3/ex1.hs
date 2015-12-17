myListLength :: [a] -> Integer
myListLength [] = 0
myListLength xs = sum [1 | _ <- xs]