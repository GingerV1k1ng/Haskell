module Main where

main :: IO ()
main = putStrLn "Hello World"

takWhile :: (t -> Bool) -> [t] -> [t]
takWhile funk []     = []
takWhile funk (x:xs) = if funk x then x : takWhile funk xs else []

droWhile :: (a -> Bool) -> [a] -> [a]
droWhile funk []    = []
droWhile funk (h:t) = if funk h then droWhile funk t else h:droWhile funk t

factors :: Integral t => t -> [t]
factors n = [i | i <- [1..n], mod n i == 0 ]

prime :: Integral a => a -> Bool
prime n = length(factors n) == 2

data Nat = Zero | Succ(Nat) deriving Show

nat2int :: Num t => Nat -> t
nat2int Zero      = 0
nat2int (Succ(n)) = 1 + nat2int n

int2nat :: (Num t, Eq t) => t -> Nat
int2nat 0 = Zero
int2nat n = Succ(int2nat n)

-- fib usng lazy evaluation

magic 0 _ = []
magic m n = m : (magic n (m+n))

getIt :: [Int] -> Int -> Int
getIt []     _ = undefined
getIt (x:xs) 1 = x
getIt (x:xs) n = getIt xs (n-1)

fib n = getIt (magic 1 1) n

--check if tree is balanced
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

badTree = Node (Leaf 5) (Node (Leaf 5) (Node (Leaf 17) (Leaf 18)))
goodTree = Node (Leaf 5) (Node (Leaf 5)  (Leaf 17))

<<<<<<< HEAD
balanced :: Tree t -> Bool
balanced (Leaf a)     = True
balanced (Node a1 a2) = balanced a1 == balanced a2
=======
balanced (Leaf a) = True
balanced (Node a b) = let x = leaves a in let y = leaves b in diff x y && balanced a && balanced b

leaves (Leaf n) = 1
leaves (Node a b) = leaves a + leaves b

diff n m = abs(n - m) <= 1

-- the start of constructing a balanced tree from a list

balance xs = (Leaf xs)
balance (x:xs) = let l = length (x:xs) in if l > 2 then Node (balance take l/2 (x:xs)) (balance drop l/2 (x:xs))  else (Node (Leaf x) (Leaf xs))
>>>>>>> 45f13495bfd44334aca1a2a2d86ac822c1e89290
