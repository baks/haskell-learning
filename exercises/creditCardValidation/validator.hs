import Test.QuickCheck

lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

toDigits :: Integer -> [Integer]
toDigits n
    | n < 0 = []
    | n == 0 = []
    | otherwise = toDigits ((n- lastDigit n) `div` 10) ++ (lastDigit n : [])

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

doubleEveryOtherFromHead :: [Integer] -> [Integer]
doubleEveryOtherFromHead [] = []
doubleEveryOtherFromHead [x] = [x]
doubleEveryOtherFromHead (x:(y:xs)) = [x,y*2] ++ doubleEveryOtherFromHead xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOtherFromHead (reverse xs))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs 

validate :: Integer -> Bool
validate xs = sumDigits (doubleEveryOther (toDigits xs)) `mod` 10 == 0

prop_idempotent xs = lastDigit (lastDigit xs) == lastDigit xs
prop_last (NonNegative xs) = [last (show xs)] == show (lastDigit xs)
prop_minusLastDivisibleByTen (NonNegative xs) = (xs - lastDigit xs) `mod` 10 == 0

sumMultipliedPairs :: [(Integer, Integer)] -> Integer
sumMultipliedPairs [] = 0
sumMultipliedPairs xs = fst (head xs) * snd (head xs) + sumMultipliedPairs (tail xs) 
prop_toDigits (NonNegative xs) = sumMultipliedPairs(zip [10^x | x <- [0..]] (reverse (toDigits xs))) == xs

prop_toDigitsRev xs = toDigitsRev xs == reverse (toDigits xs)

multiplyPair :: [(Integer, Integer)] -> [Integer]
multiplyPair [] = []
multiplyPair xs = fst (head xs) * snd (head xs) : multiplyPair (tail xs)
subtractLists :: [Integer] -> [Integer] -> [Integer]
subtractLists [] [] = []
subtractLists xs ys = [head (xs) - head (ys)] ++ subtractLists (tail xs) (tail ys)
prop_doubleEveryOtherFromHead xs = subtractLists (doubleEveryOtherFromHead xs) xs == multiplyPair(zip xs (cycle[0,1]))
