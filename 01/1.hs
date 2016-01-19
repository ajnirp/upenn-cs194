-- Exercise 1
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
          | n < 0 = []
          | n == 0 = []
          | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' [] = []
doubleEveryOther' [x] = [x]
doubleEveryOther' (x:y:xs) = x:(2*y):(doubleEveryOther' xs)

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOther' . reverse

sumDigitsSingle :: Integer -> Integer
sumDigitsSingle n
              | n < 0 = 0
              | n == 0 = 0
              | otherwise = (n `mod` 10) + sumDigitsSingle (n `div` 10)

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits = sum . map sumDigitsSingle

findCheckSum :: Integer -> Integer
findCheckSum = sumDigits . doubleEveryOther . toDigits

-- Exercise 4
validate :: Integer -> Bool
validate n = checkSum /= 0 && 0 == checkSum `mod` 10
             where checkSum = findCheckSum n

-- Exercise 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b c = [(a, b)]
hanoi n a b c = recurse1 ++ [(a, b)] ++ recurse2
    where recurse1 = hanoi (n-1) a c b
          recurse2 = hanoi (n-1) c b a

-- Exercise 6
-- TODO