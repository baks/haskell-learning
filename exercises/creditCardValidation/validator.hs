lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

toDigits :: Integer -> [Integer]
toDigits n
    | n < 0 = []
    | n == 0 = []
    | otherwise = toDigits ((n- lastDigit n) `div` 10) ++ (lastDigit n : [])  
