data Casing = IgnoreCase
            | CaseSensitive
			
globToRegex :: Casing -> String -> String

globToRegex CaseSensitive cs = '^' : globToRegex' cs ++ "$"
globToRegex IgnoreCase cs = '^' : globToRegex'' cs ++ "$"

globToRegex'' :: String -> String
globToRegex'' "" = ""

globToRegex'' ('*':cs) = ".*" ++ globToRegex'' cs

globToRegex'' ('?':cs) = '.' : globToRegex'' cs

globToRegex'' ('[':'!':c:cs) = "[^" ++ makeCaseGroup c ++ charClass' cs
globToRegex'' ('[':c:cs)     = '['  :  makeCaseGroup c ++ charClass' cs
globToRegex'' ('[':_)        = error "unterminated character class"

globToRegex'' (c:cs) = escape' c ++ globToRegex'' cs

charClass' :: String -> String
charClass' (']':cs) = ']' : globToRegex'' cs
charClass' (c:cs)   = makeCaseGroup c ++ charClass cs
charClass' []       = error "unterminated character class"

escape' :: Char -> String
escape' c | c `elem` regexChars = '\\' : [c]
         | otherwise = makeCaseGroup c
    where regexChars = "\\+()^$.{}]|"

makeCaseGroup :: Char -> String
makeCaseGroup c 
    | isLower c = '[' : c : (toUpper c) : ']' : []
    | isUpper c = '[' : (toLower c) : c : ']' : []
    | otherwise = c : []