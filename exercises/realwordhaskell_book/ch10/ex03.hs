if' :: Bool -> Parse a -> Parse a -> Parse a
if' True whenTrue _ = whenTrue
if' False _ whenFalse = whenFalse

parsePGM :: Parse (Either Greymap GreymapForP2)
parsePGM = 
    parseWhileWith w2c notWhite ==> \header -> skipSpaces ==>&
    assert (header == "P5" || header == "P2") "invalid raw header" ==>&
    if' (header == "P5") (parseRawPGM ==> \res -> identity (Left res)) (parsePlainPGM ==> \res -> identity (Right res))
  where notWhite = (`notElem` " \r\n\t")

parseRawPGM =
    parseNat ==> \width -> skipSpaces ==>&
    parseNat ==> \height -> skipSpaces ==>&
    parseNat ==> \maxGrey ->
    parseByte ==>&
    parseBytes (width * height) ==> \bitmap ->
    identity (Greymap width height maxGrey bitmap)
  where notWhite = (`notElem` " \r\n\t")
  
parsePlainPGM =
     parseNat ==> \width -> skipSpaces ==>&
     parseNat ==> \height -> skipSpaces ==>&
     parseNat ==> \maxGrey ->
     parseByte ==>&
     parseNumbers (width * height) ==> \bitmap ->
     identity (GreymapForP2 width height maxGrey bitmap)
   where notWhite = (`notElem` " \r\n\t")  