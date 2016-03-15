module GlobPlain (process,matchesGlob,Casing(..)) where

import Data.List
import Data.Maybe
import Data.Function

data Casing = IgnoreCase
            | CaseSensitive

data GlobPattern = Text String
      | Any
      | Single
      | CharacterClass String
      deriving (Eq, Ord, Show)
      
type GlobError = String
      
matchesGlob :: FilePath -> String -> Bool                                                
matchesGlob path glob 
    | isPattern glob = process path glob
    | otherwise = path == glob
    
isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

processString input Single = case inputHasAnyChar of
                               True -> Right (tail input)
                               False -> Left "Not"
                               where inputHasAnyChar = not (null input)
        
processString input Any = Right input

processString "" (Text a) = Left "Not"
processString str val@(Text a) = case startsWith of
                                     True -> Right (drop size str)
                                     False -> processString (tail str) val
                                     where size = length a
                                           startsWith = (take size str) == a

processString "" (CharacterClass a) = Left "Not"               
processString str (CharacterClass (x:'-':y:[])) = processString str (CharacterClass ([x..y]))  
processString str (CharacterClass a@('!':rest)) = if not $ (head str) `elem` a
                                                  then Right (tail str)
                                                  else Left "Not"                                               
processString str val@(CharacterClass a) = if (head str) `elem` a
                                           then Right (tail str)
                                           else processString (tail str) val

process :: String -> String -> Bool
process x pattern = case foldl step (Right x) (splitPattern pattern) of
                    Left x -> False
                    Right a -> if (null a || last pattern == '*')
                                then True
                                else False
                                           
step :: Either String String -> GlobPattern -> Either String String
step (Left x) elem = Left x
step (Right acc) elem = either (Left) (Right) (processString acc elem)
                                           
splitPattern :: String -> [GlobPattern]
splitPattern = concat . map checkIfClassesInText . (foldr (toGlobPattern') [])

singleCharacterToGlobPattern x = case x of
                                 '*' -> Any
                                 '?' -> Single
                                 _ -> Text [x]
                                 
joinCharactersIfText x a y = case x of
                          '*' -> Any : y
                          '?' -> Single : y
                          _ -> Text (x : a) : (tail y)

substr startIdx val= drop startIdx val
substr' startIdx length val = take length (drop startIdx val)
                    
toGlobPattern' :: Char -> [GlobPattern] -> [GlobPattern]
toGlobPattern' x y = case y of
                     (Text a: _) -> (joinCharactersIfText x a y)
                     (Single:_) -> (singleCharacterToGlobPattern x) : y
                     (Any:_) -> (singleCharacterToGlobPattern x) : y
                     [] -> (singleCharacterToGlobPattern x) : y
                     
checkIfClassesInText :: GlobPattern -> [GlobPattern]
checkIfClassesInText (Text a) = convert (groupBy ((==) `Data.Function.on` (=='[')) a)
                where convert str = case str of
                                    ("[" : clss@('!':']' : p) : rest) -> if endBracketExist
                                                        then CharacterClass (substr' 0 (endBracket + 2) clss) : convert ((substr (endBracket + 3) clss) : rest)
                                                        else error "Unterminated character class"
                                     where endBracketExist = isJust $ ']' `elemIndex` p
                                           endBracket = fromJust $ ']' `elemIndex` p
                                    ("[" : clss@(']':p) : rest) -> if endBracketExist
                                                        then CharacterClass (substr' 0 (endBracket + 1) clss) : convert ((substr (endBracket + 2) clss) : rest)
                                                        else error "Unterminated character class"
                                     where endBracketExist = isJust $ ']' `elemIndex` p
                                           endBracket = fromJust $ ']' `elemIndex` p
                                    ("[" : r : rest) -> if endBracketExist
                                                        then CharacterClass (substr' 0 endBracket r) : convert ((substr (endBracket + 1) r) : rest)
                                                        else error "Unterminated character class"
                                     where endBracketExist = isJust $ ']' `elemIndex` r
                                           endBracket = fromJust $ ']' `elemIndex` r
                                    ("" : []) -> []
                                    (q : qs) -> (Text q) : convert qs
                                    _ -> []
checkIfClassesInText x = [x]