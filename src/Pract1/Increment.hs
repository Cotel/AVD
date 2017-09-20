module Increment where
import Data.Char
--Ejercicio 1
increment :: Int ->Int
increment x = x + 1

--Ejercicio 2
equals :: Int -> Int -> Bool
equals x y = x == y

--Ejercicio 3
equalzero :: Int -> Bool
equalzero 0 = True
equalzero x = False

--Ejercicio 4
equalzerocond :: Int -> Bool
equalzerocond x = if x == 0 then True else False

--Ejercicio 5
elementonpos :: [a]-> Int -> a
elementonpos (x:xs) 0 = x
elementonpos xs y = elementonpos (tail xs) (y-1)  

--Ejercicio 6
stringord :: String -> [Int]
stringord x = map (ord) x