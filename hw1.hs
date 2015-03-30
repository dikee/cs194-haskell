-- Exercise 1

-- find the digits of a number
toDigits     :: Integer -> [Integer]
toDigits n
    | n < 1             = []
    | n < 10            = [n]
    | n == 10           = [1, 0]
    | n `mod` 10 == 0   = toDigits (div n 10) ++ [0]
    | otherwise         = toDigits (n `div` 10) ++ [n `mod` 10]


-- find the reverse digits of a number
toDigitsRev  :: Integer -> [Integer]
toDigitsRev n = reverse $ toDigits n


-- Exercise 2

-- Double every other digit in list starting from the right
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n
    | n == [] = []
    | mod (length n) 2 == 0 = zipWith (*) n $ cycle [2, 1]
    | otherwise = zipWith (*) n $ cycle [1, 2]


-- Exercise 3

-- Sum all digits of all numbers in a list
-- Eg [12, 3, 4] = 1 + 2 + 3 + 4 = 10
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs


-- Exercise 4

-- Check whether a credit card is valid
validate :: Integer -> Bool
validate 0 = False
validate n = rem (sumDigits (doubleEveryOther (toDigits n))) 10 == 0
