module Pract1.Pract1 (
    increment,
    areEqual,
    isZero,
    isZeroGuarded,
    nthElement,
    nthElement',
    nthElement'',
    strToAscii,
    positionInList,
    findPairThatSums
) where

import Data.Char
import Data.List

-- Ejercicio 1

increment :: Int -> Int
increment x = x + 1

-- Ejercicio 2. Define una función que, dados dos valores enteros, dé como resultado
-- si los dos valores son iguales o no.

areEqual :: Int -> Int -> Bool
areEqual x y = x == y

-- Ejercicio 3. Define la función que, dado un entero, devuelve cierto en caso de que
-- sea igual a 0 y falso en caso de que sea cualquier otro valor. Se debe usar la
-- estrategia de pattern matching.

isZero :: Int -> Bool
isZero 0 = True
isZero y = False

-- Ejercicio 4. Reescribe la misma función que en el ejercicio anterior pero usando
-- el esquema de las guardas

isZeroGuarded :: Int -> Bool
isZeroGuarded x 
    | x == 0    = True
    | otherwise = False

-- Ejercicio 5. Escribe una función que, dada una lista y un número entero, devuelva
-- el elemento de la lista que se encuentra en la posición indicada por el entero.
-- Identifica el caso base y el paso de recursión antes de comenzar a escribir.

-- Alternativa 1

nthElement :: [a] -> Int -> a
nthElement xs pos = getNthElement xs pos 0

getNthElement :: [a] -> Int -> Int -> a
getNthElement (x:xs) n pos = if n == pos then x else getNthElement xs n (pos+1)

-- Alternativa 2

nthElement' :: [a] -> Int -> a
nthElement' (x:xs) 0 = x
nthElement' (x:xs) y = nthElement' xs (y-1)

-- Alternativa haciendo uso de Maybe
-- En este caso tiene mas sentido comprobar que el entero es <= que la 
-- longitud de la lista, pero @nhemesy queria usar Maybe

nthElement'' :: [a] -> Int -> Maybe a
nthElement'' [] _ = Nothing
nthElement'' (x:xs) 0 = Just x
nthElement'' (x:xs) y = nthElement'' xs (y-1)

-- Ejercicio 6. Sabiendo que existe una función ord (disponible al importar el módulo Char)
-- que toma como argumento un Char y devuelve el código ASCII que
-- lo representa, define una función que tome como valor de entrada un String y
-- devuelva la lista de códigos ASCII que lo representan. Se debe usar la función map.

strToAscii :: String -> [Int]
strToAscii x = map ord x

-- Experimentos
-- Metodo que dado un elemento debe devolver su posicion en una lista

positionInList :: Eq a => [a] -> a -> Maybe Int
positionInList xs elem = positionInList' xs elem 0

positionInList' :: Eq a => [a] -> a -> Int -> Maybe Int
positionInList' (x:xs) elem pos = if x == elem then Just pos else positionInList' xs elem (pos+1)
positionInList' [] _ _ = Nothing

-- Metodo que dado un numero y una lista de numeros devuelva la primera pareja
-- de numeros de la lista que suman el primer parametro.
-- Sacado de esta prueba de entrevista para Google https://www.youtube.com/watch?v=XKu_SEDAykw

findPairThatSums :: (Num a, Ord a, Eq a) => [a] -> a -> Maybe (a, a)
findPairThatSums [] _ = Nothing
findPairThatSums [x] _ = Nothing
findPairThatSums list sum
    | x + y == sum = Just (x, y)
    | x + y < sum = findPairThatSums (tail ordList) sum
    | otherwise = findPairThatSums (init ordList) sum
        where 
            x = head ordList
            y = last ordList
            ordList = sort list
