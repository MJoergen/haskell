type Peg = String
type Move = (Peg, Peg)

roundFloat :: Float -> Integer
roundFloat f = floor(f + 0.5)

isqrt :: Integer -> Integer
isqrt = roundFloat . sqrt . fromIntegral

optimK :: Integer -> Integer
optimK n = n - (isqrt (2*n+1)) + 1

-- Move n discs from peg a to peg b using peg c and peg d as temporary
-- This is the Frame-Stewart algorithm
hanoi :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c d
   | n <= 0    = []
   | d == ""   = (hanoi (n-1) a c b "") ++ [(a,b)] ++ (hanoi (n-1) c b a "")
   | otherwise = (hanoi (optimK n) a c b d) ++ (hanoi (n-(optimK n)) a b d "") ++ (hanoi (optimK n) c b a d)

moveListLength :: [Move] -> Integer
moveListLength []     = 0
moveListLength (_:xs) = 1 + moveListLength xs

main = print (moveListLength (hanoi 15 "a" "b" "c" "d"))
--main = print (optimK 1, optimK 2, optimK 3, optimK 4)
