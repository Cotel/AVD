module Pract2.Pract2  (
    miInsert,
    miOrdered,
    Queue,
    empty,
    isEmpty,
    add,
    front,
    remove
) where

miInsert :: (Ord a) => a -> [a] -> [a]
miInsert x [] = [x]
miInsert x (y:ys) 
    | x < y = x : y : ys
    | otherwise = y : miInsert x ys

miOrdered :: (Ord a) => [a] -> Bool
miOrdered [] = True
miOrdered [x] = True
miOrdered (x:y:ys) = x <= y && miOrdered (y:ys)

-- Ejercicio 14
-- Definicion tipo Queue

type Queue a = [a]

empty :: Queue a
empty = []

isEmpty :: Queue a -> Bool
isEmpty [] = True
isEmpty _ = False

add :: a -> Queue a -> Queue a
add n [] = [n]
add n q = q++[n]

front :: Queue a -> a
front [] = error "Queue is empty"
front (q:qs) = q

remove :: Queue a -> Queue a
remove [] = error "Queue is empty"
remove (q:qs) = qs