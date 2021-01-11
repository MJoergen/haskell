hailstone :: Integer -> Integer
hailstone n
   | n `mod` 2 == 0 = n `div` 2
   | otherwise      = 3*n+1

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq n
   | n == 1    = [1]
   | otherwise = n : hailstoneSeq (hailstone n)
--hailstoneSeq 1 = [1]
--hailstoneSeq n = n : hailstoneSeq (hailstone n)

intListLength :: [Integer] -> Integer
intListLength []     = 0
intListLength (_:xs) = 1 + intListLength xs

hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n)

main = print (hailstoneLen 19, hailstoneLen 29)
