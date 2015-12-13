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

