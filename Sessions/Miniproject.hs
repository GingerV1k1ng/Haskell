module Main where

main :: IO ()
main = putStrLn "Hello World"

type X = String
type C = Rational
type K = Word

data E = P | E :+: E | E :*: E | E :/: E | Deriving(E) | Sin(E) | Cos(E) deriving Show

data P = X | Times C P | Mult P P | X:^:K


addition  = E :+: E
