module WholeMealProgramming where

-- fun1' it takes a list of integer
-- 
fun1' :: [Integer] -> Integer
fun1' [] = 1
fun1' (x:xs)
    | even x    = (x-2) * fun1' xs
    | otherwise = fun1' xs

fun2' :: Integer -> Integer
fun2' 1 = 0
fun2' n | even n    = n + fun2' (n `div` 2)
        | otherwise = fun2' (3 * n + 1)


fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even
        

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (/= 1) . iterate (\n -> if even n then n `div` 2 else 3*n+1)

{-| Exercise 2: Folding with trees
Recall the definition of a binary tree data structure. The height of
Binary_tree a binary tree is the length of a path from the root to the deepest
node. For example, the height of a tree with a single node is 0; the
height of a tree with three nodes, whose root has two children, is 1;
and so on. A binary tree is balanced if the height of its left and right
subtrees differ by no more than 1, and its left and right subtrees are
also balanced.
You should use the following data structure to represent binary
trees. Note that each node stores an extra Integer representing the
height at that node.
|-}

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq, Ord)


treeLevel :: Tree a -> Integer
treeLevel Leaf           = 0
treeLevel (Node n _ _ _) = n

foldTree :: (Ord a) => [a] -> Tree a
foldTree = foldr insertTree Leaf
    where
        insertTree x Leaf = Node 0 Leaf x Leaf        
        insertTree x nd@(Node h l root r)
            | l > r     = Node (treeLevel newr + 1) l root newr
            | otherwise = Node (treeLevel newl + 1) newl root r
            where
                newr = insertTree x r
                newl = insertTree x l

-- Everytime I finish a function that deals in any way with traversal of tree I feel a relief.
-- TODO: I really should take a course on Data Structures and Algorithms, I feel it will make my
-- life as a programmer/developer -> software engineer easier.

-- xor :: [Bool] -> Bool
xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' fn = foldr ((:) . fn) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\a b -> f b a) base xs


{-| Exercise 4: Finding Primes |-}
sieveSundaram n = [1,3..(n*2+2)]