lastButOne :: [a] -> a
lastButOne [] = error "empty list"
lastButOne (x:[]) = error "list with one element"
lastButOne (x:y:[]) = x
lastButOne xs = lastButOne (tail xs)
