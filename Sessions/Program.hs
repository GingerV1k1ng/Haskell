--Thor Steen Jensen
--tjen14@student.aau.dk
module Main where

main :: IO ()
main = do
  putStrLn "Enter expression"
  input <- getLine
  printAll input
  main

type X = String
type Constant = Rational
type UnsignInt = Word

data E = Ex P | E :+: E | E :*: E | E :/: E | Deriving(E) | Sin(E) | Cos(E) deriving (Show,Eq,Read)

data P = C Constant | X | Constant :**: P | P :++: P | P:^:UnsignInt deriving (Show,Eq,Read)

--Reductions
reduction :: E -> E
--Addition Reductions Rasmus Bilgram hjalp med den første reduction
reduction (Ex(C x)) = Ex(C x)
reduction (Ex X) = Ex X
reduction (Ex X :+: Ex X)                             = Ex(2 :**: X)
reduction (Ex(c1 :**: X) :+: Ex X)                    = Ex((c1+1) :**: X)
reduction (Ex(c1 :**: X) :+: Ex(c2 :**: X))           = Ex((c1+c2) :**: X)
reduction ((Sin(Ex X) :*: Sin(Ex X)) :+: (Cos(Ex X) :*: Cos(Ex X))) = Ex(C 1)
reduction ((Cos(Ex X) :*: Cos(Ex X)) :+: (Sin(Ex X) :*: Sin(Ex X))) = Ex(C 1)
reduction (Ex(X:^:k1) :+: Ex(X:^:k2))
  | k1 == k2 = Ex(2 :**: X:^:k2)
  | otherwise = Ex(X:^:k1) :+: Ex(X:^:k2)

reduction (Ex(X:^:k1) :+: Ex(c1 :**: (X:^:k2)))
  |k1 == k2 = Ex((c1 + 1) :**: (X:^:k2))
  |otherwise = Ex(X:^:k1) :+: Ex(c1 :**: (X:^:k2))

reduction (Ex(c1 :**: X:^:k1) :+: Ex(c2 :**: (X:^:k2)))
  |k1 == k2 = Ex((c1 + c2) :**: (X:^:k2))
  |otherwise = Ex(c1 :**: X:^:k1) :+: Ex(c2 :**: (X:^:k2))

reduction (x :+: (z :+: y))                           = reduction (x :+: reduction(z :+: y))
reduction (x :+: (z :*: y))                           = reduction (x :+: (reduction(z :*: y)))
reduction (Sin(x) :+: Sin(y))                         = Sin(reduction x) :+: Cos(reduction y)
reduction ((c1 :*: Sin(Ex X)) :+: Sin(Ex X))          = Ex(C 2) :*: (c1 :*: Sin(Ex X))
reduction (Cos(x) :+: Cos(y))                         = Cos(reduction x) :+: Cos(reduction y)
reduction ((c1 :*: Cos(Ex X)) :+: Cos(Ex X))          = Ex(C 2) :*: (c1 :*: Cos(Ex X))


reduction (Sin(x) :+: Cos(y)) = Sin(reduction x) :+: Cos(reduction y)
reduction (Cos(x) :+: y)      = (Cos(reduction x) :+: reduction y)
reduction (Sin(x) :+: y)      = (Sin(reduction x) :+: reduction y)
reduction (x :+: y)           = reduction(y :+: x)

--Multiplication Reductions
reduction (Ex(C 0) :*: Ex X) = Ex(C 0)
reduction (Ex(C 1) :*: Ex(X:^:k1)) = Ex(X:^:k1)
reduction (Ex(C 1) :*: x)    = reduction x
reduction (Ex X :*: Ex X )   = Ex(X:^:2)
reduction (Ex(c1 :**: X) :*: Ex X)
  | c1 == 0 = Ex(C 0)
  | otherwise = Ex(c1 :**: X:^:2)
reduction (Ex(c1 :**: X) :*: Ex(c2 :**: X))
  | c1 == 0 = Ex(C 0)
  | c2 == 0 = Ex(C 0)
  | otherwise = Ex((c1 + c2) :**: X:^:2)

reduction (x :*: (z :*: y)) = reduction(x :*: reduction(z :*: y))
reduction (x :*: (z :+: y)) = reduction(reduction(x :*: z) :*: reduction(z :+: y))
reduction (Cos(x) :*: Cos (y))  = Cos(reduction x) :*: Cos(reduction y)
reduction (Sin(x) :*: Sin (y))  = Sin(reduction x) :*: Sin(reduction y)
reduction (Sin(x) :*: Cos (y))  = Sin(reduction x) :*: Cos(reduction y)
reduction (Ex X :*: Ex(X:^:k1)) = Ex(X:^:(k1+1))
reduction (Ex(X:^:k1) :*: Ex(X:^:k2))    = Ex(X:^:(k1+k2))
reduction (Ex(X:^:k1) :*: Ex(c2 :**: X)) = Ex(c2 :**: X:^:(k1+1))
reduction (x :*: Ex(X:^:k1))   = reduction(x :*: reduction(Ex(X:^:k1)))
reduction (Cos(x) :*: y)       = Cos(reduction x) :*: reduction y
reduction (Sin(x) :*: y)       = Sin(reduction x) :*: reduction y
reduction (x :*: y)            = reduction (y :*: x)

reduction (Ex(c1 :**: X))
  |c1 == 0 = Ex(C 0)
  |c1 == 1 = Ex X
  |otherwise = Ex(c1 :**: X)

reduction (Ex(x :**: (C y))) = Ex(x :**: (C y))
reduction (Ex(x :**: (y :**: z))) = Ex(x :**: (y :**: z))

--Devision Reductions Inspiration by Marius Nørgaard
reduction (Ex(X:^:k1) :/: Ex(X:^:k2))
  | k1 - k2 >= 0 = reduction(Ex(X:^:(k1-k2)))
  |otherwise = error "Sorry not a valid devision, it has to give a possitive number"
reduction (e1 :/: e2)
  | e1 == e2 = Ex (C 1)
  |(reduction(e1) == e1) && (reduction(e2) == e2) = (e1 :/: e2)
  | otherwise = reduction(reduction(e1) :/: reduction(e2))

--Power Reductions
reduction (Ex(p:^:k))
  |k == 0 = Ex (C 1)
  |k == 1 = Ex p
  |otherwise = Ex(p:^:k)

--Deriviation Reductions nogle cases er blevet forslået af Per Nielsen
reduction (Deriving(Ex (C x)))        = Ex(C x)
reduction (Deriving(Ex X))            = Ex X
reduction (Deriving(Ex(X:^:1)))       = Ex(C 1)
reduction (Deriving(Ex(X:^:2)))       = Ex(2:**:X)
reduction (Deriving(Ex(X:^:c1)))      = Ex(toRational c1 :**:(X:^:(c1-1)))
reduction (Deriving(Sin(Ex X)))       = Cos(Ex X)
reduction (Deriving(Sin(Ex(X:^:0))))  = Ex(C 0)
reduction (Deriving(Sin(Ex(X:^:1))))  = Cos(Ex X)
reduction (Deriving(Sin(Ex(X:^:c1)))) = Ex(toRational c1 :**: (X:^:(c1-1))) :*: Cos(Ex(X:^:c1))
reduction (Deriving(Cos e)) = (Ex(C (-1)) :*: (Sin(e)))
reduction (Deriving(Ex(C (-1)) :*: (Sin(e)))) = (Ex(C (-1)) :*: (Cos (e)))
reduction (Deriving(Ex(C (-1)) :*: (Cos(e)))) = (Ex(C (-1)) :*: (Sin (e)))
reduction (Deriving(x :+: y))                 = (reduction(Deriving(x)) :+: (reduction(Deriving(y))))
reduction (Deriving(Deriving(y)))             = reduction(Deriving(reduction(Deriving(y))))
reduction (Deriving(x :*: y))                 = (reduction(Deriving(x)) :*: (reduction(Deriving(y))))
reduction (Deriving(Ex(X :++: x)))            = Ex(C 2)
reduction (Deriving(Ex(x :**: X)))            = Ex(C 0)
reduction (Deriving(Ex(c1 :++: c2)))          = Ex(C 0)
reduction (Deriving(Ex(x :**: (C y))))        = Ex(C (y*x))


--Prettyprint og commandline har taget meget Inspiration fra Rasmus Bilgram
reducePrint::E -> String
reducePrint e = prettyPrint e ++ "->" ++ prettyPrint(reduction e)

prettyPrint::E -> String
prettyPrint(e1 :+: e2) = prettyPrint e1 ++ "+" ++ prettyPrint e2
prettyPrint(e1 :*: e2) = prettyPrint e1 ++ "*" ++ prettyPrint e2
prettyPrint(Ex e) = prettyPrintPol e
prettyPrint(e1 :/: e2) = "\\frac{" ++ prettyPrint e1 ++ "}{" ++ prettyPrint e2 ++ "}"
prettyPrint(Deriving(e)) = "\\frac{d}{dx}(" ++ prettyPrint e ++ ")"
prettyPrint(Sin(e)) = "\\sin(" ++ prettyPrint e ++ ")"
prettyPrint(Cos(e)) = "\\cos(" ++ prettyPrint e ++ ")"

prettyPrintPol:: P -> String
prettyPrintPol(C c)        = show (fromRational c)
prettyPrintPol X           = show X
prettyPrintPol(c :**: p)   = prettyPrintPol (C c) ++ "*" ++ prettyPrintPol p
prettyPrintPol(p1 :++: p2) = prettyPrintPol p1 ++ "+" ++ prettyPrintPol p2
prettyPrintPol(p:^:k)      = prettyPrintPol p ++ "^{" ++ show k ++ "}"

printAll :: String -> IO()
printAll s = putStrLn(reducePrint (inputExpression s))

inputExpression :: String -> E
inputExpression = read

--Testcases
testcase3  = Ex(X:^:5) :*: Ex(X:^:3)
testcase4  = Ex(X:^:5) :/: Ex(X:^:3)
testcase5  = ((Cos(Ex X) :*: Cos(Ex X)) :+: (Sin(Ex X) :*: Sin(Ex X)))
testcase6  = Ex X :+: Ex X
testcase7  = Ex X :*: Ex X
testcase8  = (Ex X :+: Ex X) :+: Ex X
testcase9  = Ex X :*: Ex X
testcase10 = (Ex X :*: Ex X) :*: Ex X
testcase11 = Ex X :*: (Ex X :+: Ex X)
testcase12 = Ex X :*: Ex(C 1)
testcase13 = (((Ex(C 1) :*: ((Cos(Ex X) :*: Cos(Ex X)) :+: (Sin(Ex X) :*: Sin(Ex X)))) :*: Ex(X:^:3)) :/: Ex(X:^:2))
testcase15 = Ex(C 1) :*: Ex(X:^:3)
testcase16 = (Ex(C 1) :*: ((Cos(Ex X) :*: Cos(Ex X)) :+: (Sin(Ex X) :*: Sin(Ex X))))
testcase17 = ((Ex(C 1) :*: ((Cos(Ex X) :*: Cos(Ex X)) :+: (Sin(Ex X) :*: Sin(Ex X)))) :/: Ex(X:^:2)) -- :*: (Ex(X:^:3)))
testcase14 = Deriving(Ex(X:^:4) :+: Deriving(Cos(Ex X)))
