module Golf where

import Data.List

{-| Exercise 1: The output of skips is a list of lists. The first list in the output should
be the same as the input list. The second list in the output should
contain every second element from the input list. . . and the nth list in
the output should contain every nth element from the input list.

For example:
skips "ABCD" == ["ABCD", "BD", "C", "D"]
skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
skips [1] == [[1]]
skips [True,False] == [[True,False], [False]]
skips [] == [] -}

skips :: [a] -> [[a]]
skips l = [ [ x | (n, x) <- s, n `mod` k == 0 ] | (k, _) <- s ]
    where s = zip [1 ..] l

{-| Exercise 2: Local Maxima
A local maximum of a list is an element of the list which is strictly
greater than both the elements immediately before and after it. For
example, in the list [2,3,4,1,5], the only local maximum is 4, since
it is greater than the elements immediately before and after it (3 and
1). 5 is not a local maximum since there is no element that comes
after it.
-}

localMaxima :: [Integer] -> [Integer]
localMaxima l = [ y | (x : y : z : _) <- tails l, x < y && y > z ]

{-| Exercise 3: Histogram
For this task, write a function which takes as input a list of Integers between 0 and 9 (inclusive),
and outputs a vertical histogram showing how many of each number
were in the input list. You may assume that the input list does not
contain any numbers less than zero or greater than 9 (that is, it does
not matter what your function does if the input does contain such
numbers). Your output must exactly match the output shown in the
examples below.

histogram [1,1,1,5] ==
*
*
* *
==========
0123456789

histogram [1,4,5,4,6,6,3,4,2,4,9] ==
*
*
* *
****** *
==========
0123456789

I had a pretty good solution (small) for this which got deleted when I decided to take everything from 0 again. (A bit too complicated)
Future Reference: Setup functions: createGridStatic, createGrid, setColumnTo (obviously use shorter names.)
-}


-- CreateBars: Extract frequencies and create bar (that's an understatement.)
cbs :: [Integer] -> [[String]]
cbs ns = map (cb l) ns
  where
    l = length ns
    cb m n | n > 0 = ['*'] : cb (m - 1) (n - 1)
           | m > 0 = [' '] : cb (m - 1) n
           | True = [] 

histogram :: [Integer] -> String
histogram xs =
    "\n"
        ++ intercalate "\n" (filter (/= "          ") (map concat (cw bs)))
        ++ "\n==========\n0123456789\n"
  where
    cw = reverse . transpose
    bs = cbs (fs xs)
    fs nts = [ fromIntegral (length (filter (i ==) nts)) | i <- [0 .. 9] ]