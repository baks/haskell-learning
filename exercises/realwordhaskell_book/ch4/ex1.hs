safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead _ = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (x:xs) = Just xs
safeTail _ = Nothing

safeLast :: [a] -> Maybe a
safeLast xs = safeHead (reverse xs)

safeInit :: [a] -> Maybe [a]
safeInit (x:xs) = Just (init (x:xs))
safeInit _ = Nothing