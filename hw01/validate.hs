import Data.Char
--01. create a toDigits function and a toDigitsRev function
toDigits :: Integer -> [Integer]
toDigits num = foldr (\el acc -> (unString el):acc) [] (show num)

unString :: Char -> Integer
unString = toInteger . digitToInt
{-
-- This is, I think a more "correct" approach
toDigits num = findDigets num
  where findDigets numeral
                          | numeral < 10 = [numeral]
                          | otherwise = numeral `mod` 10 : findDigets ((numeral - (numeral `mod` 10)) / 10)
-}
toDigitsRev :: Integer -> [Integer]
toDigitsRev num = foldr (\el acc -> (unString el):acc) [] $ reverse $ show num

--02. create a doubleEveryOther function
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = applyEveryOther (*2) xs 0
  where applyEveryOther fn (y:ys) i
                                 |ys == [] = applyIfEven fn y i : ys
                                 |otherwise = applyIfEven fn y i : applyEveryOther fn ys (i + 1)

applyIfEven :: (a -> a) -> a -> Int -> a
applyIfEven fn arg i = if even i then fn arg else arg

sumDigits :: [Integer] -> Integer
sumDigits (x:xs)
                | xs == [] = x
                | x > 9 = sumDigits (toDigits x) + (sumDigits xs)
                | otherwise = x + sumDigits xs

validate :: Integer -> Bool
validate x = (getSum x `mod` 10) == 0
  where getSum = sumDigits . doubleEveryOther . toDigits
