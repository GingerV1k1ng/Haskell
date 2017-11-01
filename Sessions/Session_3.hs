module Main where

main :: IO ()
main = putStrLn "Hello World"

data Type = Int | Bool | To Type Type | Cross Type Type  deriving Show

type Var = String

data LamdaC = Vari(Var) | Cons C | Pair LamdaC LamdaC | App LamdaC LamdaC | If LamdaC LamdaC LamdaC | Lamda Var LamdaC | Let Var LamdaC LamdaC deriving Show

data C = CN | CPlus | CEqual | CIsZero | CTrue | CFalse | CNot | CFirst | CSecond deriving Show
