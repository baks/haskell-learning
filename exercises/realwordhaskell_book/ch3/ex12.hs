import Data.List

data Direction = DLeft
               | DRight
               | DStraight
                 deriving (Eq,Show)

type Point2D = (Double,Double)

slope :: Point2D -> Point2D -> Double
slope a b = (snd b - snd a) / (fst b - fst a)


sortPointsBySlope :: Point2D -> [Point2D] -> [Point2D]
sortPointsBySlope reference points = sortBy compareDesc p ++ reverse (sortBy (flip compareDesc) n)
    where p = [positive | positive <- points, (slope positive reference) >= 0] 
          n = [negative | negative <- points, (slope negative reference) < 0]
          compareDesc a b = case (slope a reference) `compare` (slope b reference) of EQ -> case fst a `compare` fst b of EQ -> snd a `compare` snd b
                                                                                                                          GT -> GT
                                                                                                                          LT -> LT
                                                                                      GT -> GT
                                                                                      LT -> LT
computeDirection :: Point2D -> Point2D -> Point2D -> Direction
computeDirection a b c
    | n > 0 = DLeft
    | n == 0 = DStraight
    | n < 0 = DRight
    where n = (fst b - fst a)*(snd c - snd a) - (snd b - snd a)*(fst c - fst a) 

processList :: [Point2D] -> [Direction]
processList (x:y:z:[]) = computeDirection x y z : []
processList (x:y:z:rest) = computeDirection x y z : processList (y:z:rest)

findMinimum :: [Point2D] -> Point2D
findMinimum points = head (sortBy yVal points)
    where yVal a b = case snd a `compare` snd b of EQ -> fst a `compare` fst b
                                                   GT -> GT
                                                   LT -> LT


grahamScan :: [Point2D] -> [Point2D]
grahamScan [x] = [x]
grahamScan points = if anyRight then grahamScan(min : actual) else min : actual
    where min = findMinimum points
          actual = [snd hull | hull <- hullIndexed]
          anyRight = DRight `elem` directions
          hullIndexed = [h | h <- indexed, fst h < (length sorted), head (drop (fst h) directions) == DLeft]
          indexed = zip [0..] sorted
          sorted = sortPointsBySlope min points
          directions = processList (min : sorted ++ [min])