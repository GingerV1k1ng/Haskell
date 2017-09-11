type Operator = Double -> Double -> Double
type Entry = (String, Operator)
type Register = [Entry]

operatorRegister :: Register -- The :: operator means "is of type"
operatorRegister = [
                ("+", (+)),
                ("*", (*)),
                ("/", (/)),
                ("-", (-))
              ]

main = do
  math <- getLine
  print $ calculate math

calculate :: String -> Double
calculate = eval operatorRegister . words

eval :: Register -> [String] -> Double
eval _ [number] = read number
eval ((operator, function):rest) unparsed =
  case span (/=operator) unparsed of
    (_, []) -> eval rest unparsed
    (beforeOperator, afterOperator) ->
      function
        (eval operatorRegister beforeOperator)
        (eval operatorRegister $ drop 1 afterOperator)
