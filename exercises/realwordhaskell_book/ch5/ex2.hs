nest :: Int -> Doc -> Doc
nest width x = (nestDoc 0 (x:[]))
    where nestDoc level (d:ds) =
            case d of
            Empty -> nestDoc level ds
            Char '{' -> d <> nestDoc (level+1) (Line:ds)
            Char '}' -> d <> nestDoc (level-1) (Line:ds)
            Char '[' -> d <> nestDoc (level+1) ds
            Char ']' -> d <> nestDoc (level-1) ds
            Char c -> d <> nestDoc level ds
            Text s -> d <> nestDoc level ds
            Line -> d <> repeatSpaces 0 (width * (computeLevel level ds)) <> nestDoc (computeLevel level ds) ds
            a `Concat` b -> nestDoc level (a:b:ds)
            a `Union` b -> nestDoc level (a:ds) `Union` nestDoc level (b:ds)
          nestDoc _ [] = Empty

computeLevel :: Int -> [Doc] -> Int
computeLevel level (x:xs) =
             case x of
             Char '}' -> level - 1
             Char ']' -> level - 1
             Empty -> level
             Text s -> level
             Line -> level
             _ -> level
computeLevel _ _ = 0

repeatSpaces :: Int -> Int -> Doc
repeatSpaces actual toBe
    | actual < toBe = Text (replicate (toBe - actual) 'a')
    | otherwise = Empty