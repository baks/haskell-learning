if' :: Bool -> Parse a -> Parse a -> Parse a
if' True whenTrue _ = whenTrue
if' False _ whenFalse = whenFalse

parseSinglePixelFormat width height = parseBytes (width * height)
parseDoublePixelFormat width height = parseBytes (width * height * 2)

parseRawPGM' =
    parseWhileWith w2c notWhite ==> \header -> skipSpaces ==>&
    assert (header == "P5") "invalid raw header" ==>&
    parseNat ==> \width -> skipSpaces ==>&
    parseNat ==> \height -> skipSpaces ==>&
    parseNat ==> \maxGrey ->
    parseByte ==>&
    if' (maxGrey < 256) (parseSinglePixelFormat width height) (parseDoublePixelFormat width height) ==> \bitmap ->
    identity (Greymap width height maxGrey bitmap)
  where notWhite = (`notElem` " \r\n\t")