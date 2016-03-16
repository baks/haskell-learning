import ControlledVisit (Info(..), getInfo)
import Data.Maybe

equalP :: (Eq a) => (Info -> Maybe a) -> a -> (Info -> Bool)
equalP f k info = (fromJust (f info)) == k

liftP :: (a -> b -> c) -> (Info -> a) -> b -> (Info -> c)
liftP q f k info = f info `q` k

greaterP, lesserP :: (Ord a) => (Info -> a) -> a -> (Info -> Bool)
greaterP = liftP (>)
lesserP = liftP (<)

simpleAndP :: (Info -> Bool) -> (Info -> Bool) -> Info -> Bool
simpleAndP f g info = f info && g info

liftP2 :: (a -> b -> c) -> (Info -> a) -> (Info -> b) -> (Info -> c)
liftP2 q f g info = f info `q` g info

andP = liftP2 (&&)
orP = liftP2 (||)

constP :: a -> Info -> a
constP k info = k

liftP' q f k info = liftP2 q f (constP k info)

some = do 
          info <- getInfo "ex01.hs"
          print (infoSize `equalP` 349 $ info)