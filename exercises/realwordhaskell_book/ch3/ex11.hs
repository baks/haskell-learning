data Direction = DLeft
               | DRight
               | DStraight
                 deriving Show

type Point2D = (Double,Double)

computeDirection :: Point2D -> Point2D -> Point2D -> Direction
computeDirection a b c
    | n > 0 = DRight
    | n == 0 = DStraight
    | n < 0 = DLeft
    where n = (fst b - fst a)*(snd c - snd a) - (snd b - snd a)*(fst c - fst a) 

processList :: [Point2D] -> [Direction]
processList (x:y:z:[]) = computeDirection x y z : []
processList (x:y:z:rest) = computeDirection x y z : processList (y:z:rest)