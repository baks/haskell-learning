{-- snippet module --}
module Glob (namesMatching) where
{-- /snippet module --}

{-- snippet import.rest --}
import Control.Exception (handle)
import Control.Monad (forM)
import GlobRegex (matchesGlob, Casing(..))
{-- /snippet import.rest --}
{-- snippet import.directory --}
import System.Directory (doesDirectoryExist, doesFileExist, makeAbsolute,
                         getCurrentDirectory, getDirectoryContents)
{-- /snippet import.directory --}
{-- snippet import.filepath --}
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>), pathSeparator, combine)
{-- /snippet import.filepath --}

windowsSeparator = '\\'
unixSeparator = '/'

{-- snippet type --}
namesMatching :: String -> IO [FilePath]
{-- /snippet type --}

{-- snippet namesMatching --}
isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

namesMatching pat
  | not (isPattern pat) = do
    exists <- doesNameExist pat
    return (if exists then [pat] else [])
{-- /snippet namesMatching --}

{-- snippet namesMatching2 --}
  | otherwise = do
    case splitFileName pat of
      ("./", baseName) -> do
          curDir <- getCurrentDirectory
          let matchesFunc = if recursive pat
                            then listMatches'
                            else listMatches
          matchesFunc curDir baseName
      (dirName, baseName) -> do
          dirs <- if isPattern dirName
                  then namesMatching (dropTrailingPathSeparator dirName)
                  else return [dirName]
          let listDir = if isPattern baseName
                        then if recursive pat
                             then listMatches'
                             else listMatches
                        else listPlain
          pathNames <- forM dirs $ \dir -> do
                           baseNames <- listDir dir baseName
                           return (map (dir </>) baseNames)
          return (concat pathNames)
{-- /snippet namesMatching2 --}

{-- snippet listMatches --}
listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
    dirName' <- if null dirName
                then getCurrentDirectory
                else return dirName
    handle ((const (return [])) :: IOError -> IO [String]) $ do
        names <- getDirectoryContents dirName'
        let names' = if isHidden pat
                     then filter isHidden names
                     else filter (not . isHidden) names
        let casing = if pathSeparator == windowsSeparator
                     then IgnoreCase
                     else CaseSensitive
        let match name pat = matchesGlob name casing pat
        return (filter (`match` pat) names')

listMatches' :: FilePath -> String -> IO [String]
listMatches' dirName pat = do
    dirName' <- if null dirName
                then getCurrentDirectory
                else return dirName
    handle ((const (return [])) :: IOError -> IO [String]) $ do
        names <- getDirectoryContentsRecursive dirName'
        let names' = if isHidden pat
                     then filter isHidden names
                     else filter (not . isHidden) names
        let casing = if pathSeparator == windowsSeparator
                     then IgnoreCase
                     else CaseSensitive
        let match name pat = matchesGlob name casing pat
        return (filter (`match` pat) names')

isHidden ('.':_) = True
isHidden _       = False

recursive ('*' : '*' : _) = True
recursive _ = False

getDirectoryContentsRecursive :: FilePath -> IO [FilePath]          
getDirectoryContentsRecursive path = do
                                     contents <- getDirectoryContents path
                                     names <- mapM traverseDir (map (path </>) contents)
                                     return (concat names)

traverseDir :: FilePath -> IO [FilePath]
traverseDir path = do
                 exists <- doesDirectoryExist path
                 if exists && not (isSpecialDir (take 2 (reverse path))) then 
                     do
                     getDirectoryContentsRecursive path
                 else return [path]
                                    
isSpecialDir ('.' : _) = True
isSpecialDir ".." = True
isSpecialDir _ = False

{-- /snippet listMatches --}
{-- snippet listPlain --}
listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
    exists <- if null baseName
              then doesDirectoryExist dirName
              else doesNameExist (dirName </> baseName)
    return (if exists then [baseName] else [])
{-- /snippet listPlain --}

{-- snippet doesNameExist --}
doesNameExist :: FilePath -> IO Bool

doesNameExist name = do
    fileExists <- doesFileExist name
    if fileExists
      then return True
      else doesDirectoryExist name
{-- /snippet doesNameExist --}
