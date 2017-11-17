module Main where

main :: IO ()
main = putStrLn "Hello World"

type TVar = String

data HType = Int' | Bool' | Var TVar | HType :->: HType | HType :&: HType  deriving Show

data HTSch = AType HType | All TVar HType

type TSubst = TVar -> HType
