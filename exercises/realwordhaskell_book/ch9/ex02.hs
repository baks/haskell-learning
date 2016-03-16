import ControlledVisit
import Data.List
import Data.Maybe
import System.Directory (Permissions(..), getDirectoryContents,
                         getModificationTime, getPermissions)

postorder :: [Info] -> [Info] 
postorder info = sortBy (sortI) filtered
                 where filtered = filter (\x -> (infoPath x) /= "") info
sortI x y = isDirectory y `compare` isDirectory x