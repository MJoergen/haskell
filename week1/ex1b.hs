type Peg = String
type Move = (Peg, Peg)

-- Move n discs from peg a to peg b using peg c as temporary
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
   | n <= 0    = []
   | otherwise = (hanoi (n-1) a  c  b) ++ [(a,b)] ++ (hanoi (n-1) c  b  a)

main = print (hanoi 2 "a" "b" "c")   
