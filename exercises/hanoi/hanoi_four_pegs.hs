type Peg = String
type Move = (Peg, Peg)

moveOne :: Peg -> Peg -> [Move]
moveOne src dst = [(src, dst)]

move :: Integer -> Peg -> Peg -> Peg -> [Move]
move 1 src tmp dst = moveOne src dst
move n src tmp dst = move (n-1) src dst tmp ++ moveOne src dst ++ move (n-1) tmp src dst

move' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
move' 0 src tmp1 tmp2 dst = []
move' 1 src tmp1 tmp2 dst = moveOne src dst
move' n src tmp1 tmp2 dst = 
     move' k src tmp1 dst tmp2 ++ 
     move (n-k) src dst tmp1 ++ 
     move' k tmp2 src tmp1 dst
     where k
         |n > 9 = n - 4
         |n > 4 = n - 3
         |n > 2 = n - 2
         |otherwise = n - 1