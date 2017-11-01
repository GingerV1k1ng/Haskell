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

data Tree a = Leaf a | Node (Tree a) (Tree a)

balanced :: Tree t -> Bool
balanced (Leaf a)     = True
balanced (Node a1 a2) = balanced a1 == balanced a2
