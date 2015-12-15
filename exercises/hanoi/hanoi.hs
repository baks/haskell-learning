type Peg = String
type Move = (Peg, Peg)

--hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c = move n a c b

moveOne :: Peg -> Peg -> [Move]
moveOne src dst = [(src, dst)]

move :: Integer -> Peg -> Peg -> Peg -> [Move]
move 1 src tmp dst = moveOne src dst
move n src tmp dst = move (n-1) src dst tmp ++ moveOne src dst ++ move (n-1) tmp src dst
