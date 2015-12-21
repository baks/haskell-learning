splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith predicate list = 
      let xs = takeWhile predicate list
      in case xs of
          [] -> splitWith predicate (tail list)
          ys -> ys : splitWith predicate (drop (length ys) list)