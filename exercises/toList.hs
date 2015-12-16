data List a = Cons a (List a)
            | Nil
             deriving (Show)
             

toList :: List a -> [a]
toList (Cons x xs) = [x] ++ toList xs
toList Nil = []