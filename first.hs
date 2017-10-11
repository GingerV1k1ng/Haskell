
module Main where

main :: IO ()
main = putStrLn "Hello World"


doubleMe x = x + x




doubleUs x y = doubleMe x*2 - doubleMe y*2

foo x = x + x + x + x + x + x +
        x + x + x



doubleSmallNumber x = if x > 100
then x else x*2





boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]





sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z
