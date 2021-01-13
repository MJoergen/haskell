-- Convert from integer to list of digits
-- toDigits 1234 == [1,2,3,4]
toDigits :: Integer -> [Integer]
toDigits n
   | n <= 0    = []
   | n < 10    = [n]
   | otherwise = toDigits (n `div` 10) ++ [n `mod ` 10]

-- Convert from integer to list of digits in reversed order
-- toDigitsRev 1234 == [4,3,2,1]
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
   | n <= 0    = []
   | n < 10    = [n]
   | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

-- Reverse a list
reverseList :: [Integer] -> [Integer]
reverseList []     = []
reverseList (x:xs) = reverseList(xs) ++ [x]

-- Double every other number from the left
doubleEveryOtherLeft :: [Integer] -> [Integer]   
doubleEveryOtherLeft []         = []  -- Do nothing to the empty list
doubleEveryOtherLeft (x:[])     = [x] -- A list with a single element is unchanged
doubleEveryOtherLeft (x:(y:zs)) = [x] ++ [2*y] ++ doubleEveryOtherLeft zs -- A list with a single element is unchanged

-- Double every other number from the right
doubleEveryOther :: [Integer] -> [Integer]   
doubleEveryOther xs = reverse(doubleEveryOtherLeft(reverse(xs)))

-- Sum all digits of a single number
sumNumber :: Integer -> Integer
sumNumber n
   | n <= 0    = 0
   | n < 10    = n
   | otherwise = (n `mod` 10) + sumNumber(n `div` 10)

-- Sum all digits
sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = sumNumber(x) + sumDigits(xs)

validate :: Integer -> Bool
validate n = (sumDigits(doubleEveryOther(toDigits(n))) `mod` 10) == 0

--main = print(doubleEveryOther (toDigits 5678))
--main = print(sumDigits [16,7,12,5])
main = print(validate 4012888888881881)

