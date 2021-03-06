module CreditCardValidator where

{-| Credit Card Validation:
http://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf

In this section, you will implement the validation algorithm for
credit cards. It follows these steps:
• Double the value of every second digit beginning from the right.
That is, the last digit is unchanged; the second-to-last digit is doubled;
the third-to-last digit is unchanged; and so on. For example,
[1,3,8,6] becomes [2,3,16,6].

• Add the digits of the doubled values and the undoubled digits
from the original number. For example, [2,3,16,6] becomes
2+3+1+6+6 = 18.

• Calculate the remainder when the sum is divided by 10. For the
above example, the remainder would be 8.
If the result equals 0, then the number is valid.
-}

toDigits :: Integer -> [Integer]
toDigits n
    | n > 0     = toDigits (n `div` 10) ++ [mod n 10]
    | otherwise = []

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith (*) (cycle [1,2]) . reverse

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

validate :: Integer -> Bool
validate cc = (sumDigits . doubleEveryOther . toDigits $ cc) `mod` 10 == 0
-- validate cc = (length . toDigits ) cc == 16 && (sumDigits . doubleEveryOther . toDigits $ cc) `mod` 10 == 0