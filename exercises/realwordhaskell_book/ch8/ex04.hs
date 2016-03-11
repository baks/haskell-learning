import Text.Regex.Posix ((=~))
import Data.Char

data Casing = IgnoreCase
            | CaseSensitive
            
type GlobError = String

globToRegex :: Casing -> String -> Either GlobError String

{-- snippet rooted --}
globToRegex CaseSensitive cs = either (Left) (\x-> Right ('^' : x ++ "$")) (globToRegex' cs)
globToRegex IgnoreCase cs = either (Left) (\x -> Right('^' : x ++ "$")) (globToRegex'' cs)
{-- /snippet rooted --}

{-- snippet asterisk --}
globToRegex' :: String -> Either GlobError String
globToRegex' "" = Right ""

globToRegex' ('*':cs) = either (Left) (\x -> Right (".*" ++ x)) (globToRegex' cs)

globToRegex' ('?':cs) = either (Left) (\x -> Right ('.' : x)) (globToRegex' cs)

globToRegex' ('[':'!':c:cs) = either (Left) (\x-> Right ("[^" ++ c : x)) (charClass cs)
globToRegex' ('[':c:cs)     = either (Left) (\x-> Right ('['  :  c : x)) (charClass cs)
globToRegex' ('[':_)        = Left "unterminated character class"

globToRegex' (c:cs) = either (Left) (\x-> Right ((escape c) ++ x)) (globToRegex' cs)
{-- /snippet asterisk --}

{-
{-- snippet last --}
globToRegex' (c:cs) = escape c ++ globToRegex' cs
{-- /snippet last --}
-}

{-- snippet escape --}
escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
    where regexChars = "\\+()^$.{}]|"
{-- /snippet escape --}

{-- snippet charClass --}
charClass :: String -> Either GlobError String
charClass (']':cs) = either (Left) (\x -> Right (']' : x)) (globToRegex' cs)
charClass (c:cs)   = either (Left) (\x -> Right (c : x)) (charClass cs)
charClass []       = Left "unterminated character class"
{-- /snippet charClass --}

{-- snippet matchesGlob --}
matchesGlob :: FilePath -> Casing -> String -> Either GlobError Bool
matchesGlob name casing pat = either (Left) (\x -> Right (name =~ x)) (globToRegex casing pat)
--name `matchesGlob` pat = match (makeRegex (globToRegex pat)) name
{-- /snippet matchesGlob --}

globToRegex'' :: String -> Either GlobError String
globToRegex'' "" = Right ""

globToRegex'' ('*':cs) = either (Left) (\x -> Right (".*" ++ x)) (globToRegex'' cs)

globToRegex'' ('?':cs) = either (Left) (\x -> Right ('.' : x)) (globToRegex'' cs)

globToRegex'' ('[':'!':c:cs) = either (Left) (\x -> Right ("[^" ++ makeCaseGroup c ++ x)) (charClass' cs)
globToRegex'' ('[':c:cs)     = either (Left) (\x -> Right ('['  :  makeCaseGroup c ++ x)) (charClass' cs)
globToRegex'' ('[':_)        = Left "unterminated character class"

globToRegex'' (c:cs) = either (Left) (\x-> Right ((escape c) ++ x)) (globToRegex'' cs)

charClass' :: String -> Either GlobError String
charClass' (']':cs) = either (Left) (\x -> Right (']' : x)) (globToRegex'' cs)
charClass' (c:cs)   = either (Left) (\x -> Right (makeCaseGroup c ++ x)) (charClass cs)
charClass' []       = Left "unterminated character class"

escape' :: Char -> String
escape' c | c `elem` regexChars = '\\' : [c]
         | otherwise = makeCaseGroup c
    where regexChars = "\\+()^$.{}]|"

makeCaseGroup :: Char -> String
makeCaseGroup c 
    | isLower c = '[' : c : (toUpper c) : ']' : []
    | isUpper c = '[' : (toLower c) : c : ']' : []
    | otherwise = c : []