data GreymapForP2 = GreymapForP2 {
      greyWidth :: Int
    , greyHeight :: Int
    , greyMax :: Int
    , greyData :: [Int]
    } deriving (Eq)

instance Show GreymapForP2 where
    show (GreymapForP2 w h m _) = "GreymapForP2 " ++ show w ++ "x" ++ show h ++
                             " " ++ show m
                             
parseNumbers :: Int -> Parse [Int]
parseNumbers 0 = Parse (\s -> Right ([], s))
parseNumbers n = parseNat ==> \number -> 
                 skipSpaces ==> \x -> 
                 (number:) <$> parseNumbers (n-1)
                             
parsePlainPGM =
     parseWhileWith w2c notWhite ==> \header -> skipSpaces ==>&
     assert (header == "P2") "invalid plain header" ==>&
     parseNat ==> \width -> skipSpaces ==>&
     parseNat ==> \height -> skipSpaces ==>&
     parseNat ==> \maxGrey ->
     parseByte ==>&
     parseNumbers (width * height) ==> \bitmap ->
     identity (GreymapForP2 width height maxGrey bitmap)
   where notWhite = (`notElem` " \r\n\t")  