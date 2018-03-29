module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

fibs1 :: [Integer]
fibs1 = [ fib x | x <- [1 ..] ]

fibs2' = 0 : 1 : cont fibs2'
  where
    cont []           = []
    cont (a : b : cs) = a + b : cont (b : cs)

-- or the super simple
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

streamFromList :: [a] -> Stream a
streamFromList [] =
    error ("List is not infinite, can't be converted to stream")
streamFromList (x : xs) = Cons x (streamFromList xs)

instance Show a => Show (Stream a) where
    show x = (init . show . take 20 . streamToList) x ++ ",..."


streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap fn (Cons x xs) = Cons (fn x) (streamMap fn xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed seed init = Cons init (streamFromSeed seed (seed init))

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

ruler :: Stream Integer
ruler = streamMap (nthRuler) nats

nthRuler :: Integer -> Integer
nthRuler x | value == [] = 0
           | otherwise   = head value
  where
    value = dropWhile (\z -> x `mod` (z * 2) /= 0)
                      [ y | y <- reverse [1 .. (x - 1)] ]