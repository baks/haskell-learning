turnIntoPalindrome :: [a] -> [a]
turnIntoPalindrome [] = []
turnIntoPalindrome xs = [x | x <- xs] ++ [x | x <- reverse xs]