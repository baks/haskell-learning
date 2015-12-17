isPalindrome :: [Char] -> Bool
isPalindrome [] = True
isPalindrome (x:xs) = x == (last xs) && isPalindrome (init xs)