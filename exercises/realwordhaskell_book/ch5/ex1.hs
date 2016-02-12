{-- snippet fill --}
fill :: Int -> Doc -> Doc
fill width x = hcat (fillDoc 0 [x])
    where fillDoc wh (d:ds) =
            case d of
            Empty        -> fillDoc wh ds
            Char c       -> d : fillDoc (wh + 1) ds
            Text s       -> d : fillDoc (wh + length s) ds
            Line         -> (repeatSpaces wh width) : d : fillDoc 0 ds
            a `Concat` b -> fillDoc wh (a:b:ds)
            a `Union` b  -> fillDoc wh (b:ds)
          fillDoc wh _ = repeatSpaces wh width : []
{-- /snippet fill --}

repeatSpaces :: Int -> Int -> Doc
repeatSpaces actual toBe
    | actual < toBe = Text (replicate (toBe - actual) ' ')
    | otherwise = Empty