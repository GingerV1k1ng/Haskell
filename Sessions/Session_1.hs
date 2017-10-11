
module Main where

main :: IO ()
main = putStrLn "Hello World"

sumH 1 = 1
sumH n = n + sumH(n-1)

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

prime n = not (nontrivialdivisor n (n-1))
          where
            nontrivialdivisor n 1 = False
            nontrivialdivisor n k = if (mod n k == 0) then True else (nontrivialdivisor n (k -1))


reverseL (a:b) = (reverseL b) ++ [a]

reverseH [] = []
reverseH l  = reverseH(tail l) ++ [head l]

ispalindrome lst = reverseH lst == lst

kabrok r 0 = []
kabrok r n = a : remainfrac r1
            where
              a = truncate r
              r1 = r - fromIntegral a
              remainfrac r1 = kabrok (1/r1) (n-1)

--Finds the last element
lastL lst = if length lst == 1 then head lst else lastL(tail lst)

--Finds the n'th element
lastN lst 1 = head lst
lastN lst n = lastN (tail lst) (n-1)

--flatten lst   = head lst ++ flatten(tail lst)

flatten []    = []
flatten (x:y) = x ++ flatten y
