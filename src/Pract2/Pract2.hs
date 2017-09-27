module Pract2.Pract2  (
    miInsert,
    miOrdered
) where

miInsert :: (Ord a) => a -> [a] -> [a]
miInsert x [] = [x]
miInsert x (y:ys) 
    | x < y = x : y : ys
    | otherwise = y : (miInsert x ys)

miOrdered :: (Ord a) => [a] -> Bool
miOrdered [] = True
miOrdered [x] = True
miOrdered (x:y:ys) = x <= y && miOrdered (y:ys)
