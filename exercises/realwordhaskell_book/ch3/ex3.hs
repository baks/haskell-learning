mean :: [Integer] -> Maybe Double
mean [] = Nothing
mean xs = Just (fromIntegral((sum xs) `div` (fromIntegral (length xs))))
