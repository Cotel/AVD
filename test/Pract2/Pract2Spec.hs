module Pract2.Pract2Spec where
  
import Test.Hspec
import Test.QuickCheck
import Pract2.Pract2
import Data.List

spec = describe "Pract2 tests" $ do
  it "Exercise 1.1: reverse unit is equal to unit" $
    quickCheck prop_RevUnit
  it "Exercise 1.2: reverse applied to list is equal to reversed list (Is this okay?)" $
    quickCheck prop_RevApp
  it "Exercise 1.3: reverse applied to reversed list is equal to list" $
    quickCheck prop_RevRev
  it "Exercise 6: Max takes y every time x is lower" $
    quickCheck prop_MaxLe
  it "Exercise 7: miInsert always returns an ascending ordered list" $
    quickCheck collectEx
  it "Exercise 8: Cycle is equal to concatenate with parameter itself" $
    quickCheck prop_DoubleCycle
  it "Exercise 11: miInsert always returns an ordered list" $
    quickCheck prop_miInsert


-- Ejercicios 1, 2, 3, 4 y 5
-- Si quitamos la signature de las funciones los tests pasan aunque
-- escribamos mal las funciones a proposito porque los inputs random generados son
-- del tipo unit () y () == () es correcto

prop_RevUnit :: [Int] -> Bool
prop_RevUnit x = reverse [x] == [x]

prop_RevApp :: [Int] -> [Int] -> Bool
prop_RevApp xs ys = reverse (xs++ys) == reverse ys ++ reverse xs

prop_RevRev :: [Int] -> Bool
prop_RevRev list = reverse (reverse list) == list

-- Ejercicio 6
-- Definimos un nuevo tipo de propiedad. En este caso devuelve el tipo Property,
-- que actua igual que Bool y añade el tipo Nothing.
-- Para los inputs generados se comprueba una condicion. Si la cumplen se pasa
-- a comprobar la propiedad devolviendo True o False. Si los inputs
-- no cumplen la condicion se devuelve Nothing ya que no son validos para la propiedad
-- y no se comprueba nada.

prop_MaxLe :: Int -> Int -> Property
prop_MaxLe x y = x <= y ==> max x y == y

-- Ejercicio 7
-- La siguiente propiedad hace que QuickCheck se rinda antes de encontrar solucion
-- pues no es capaz de generar casos de prueba que satisfagan la condicion

prop_Ordered :: Int -> [Int] -> Property
prop_Ordered x ys = miOrdered ys ==> miInsert x ys == sort (x:ys)

-- Ejercicio 8
-- En la siguiente propiedad se trata con una estructura de datos infinita.
-- y los tests no acabarán nunca

prop_DoubleCycleInfinite :: [Int] -> Property
prop_DoubleCycleInfinite xs = not (null xs) ==> cycle xs == cycle (xs ++ xs)

-- Sin embargo, podemos aplicar una regla matematica para poder convertir
-- el problema en finito. Si los n primeros elementos de dos listas infinitas
-- son iguales, entonces las listas son iguales.

prop_DoubleCycle :: [Int] -> Int -> Property
prop_DoubleCycle xs n = not (null xs) && n >= 0 ==> take n (cycle xs) == take n (cycle (xs ++ xs))

-- Ejercicio 9 y 10
-- Para ayudarnos en el debugging de los testeos realizados por QuickCheck podemos
-- hacer uso de funciones como "classify" o "collect" para obtener algo mas de
-- informacion sobre los casos de prueba generados.

-- Classify etiqueta los casos de pruebas que cumplen un predicado y nos informa
-- al final de la ejecucion cuantos de los casos de prueba pertenecen a este grupo.
-- El resultado de ejecutar la siguiente funcion es "Gave up! Passed only 84 tests (35% trivial)."
-- Significa que el 35% de los casos pertenecen al grupo clasificado (aquellos que tienen la lista vacia)

classifyEx :: Int -> [Int] -> Property
classifyEx x ys = miOrdered ys ==> classify (null ys) "trivial" $ miInsert x ys == sort (x:ys)

-- Collect genera un histograma que mide el resultado de un predicado para los casos generados
-- El resultado para la siguiente funcion es:
-- *** Gave up! Passed only 73 tests:
-- 38% 0
-- 34% 1
-- 17% 2
--  8% 3
--  1% 4

-- Significa que el 38% de los casos generados la lista tiene logitud 0. El 34% tiene longitud 1. Etc.

collectEx :: Int -> [Int] -> Property
collectEx x ys = miOrdered ys ==> collect (length ys) $ miInsert x ys == sort (x:ys)

-- Ejercicio 11
-- Haciendo uso de las herramientas vistas arriba hemos llegado a la conclusión
-- de que la propiedad para miInsert no se cumple porque quickcheck no genera
-- listas ordenadas como caso de prueba. Vamos a construir un generador propio
-- para poder crear casos de prueba a medida para testear la siguiente propiedad.
-- Es una version mejorada para prop_Ordered.

prop_miInsert :: Int -> Property
prop_miInsert x = forAll miOrderedList2 $ \xs -> miOrdered (miInsert x xs)

-- Se lee de la siguiente forma: Para toda lista ordenada generada, la lista sigue ordenada
-- despues de insertar un elemento con el metodo que quiero testear (insercion ordenada de elementos).

-- Para poder generar un tipo de datos de forma arbitraria, QuickCheck proporciona
-- la typeclass Arbitrary.

-- class Arbitrary a where
--   arbitrary :: Gen a 

-- Gen es un tipo de datos abstracto que representa un generador de datos para el tipo a.
-- QuickCheck trae generadores definidos para los tipos basicos. Para generar casos
-- adecuados a las necesidades, el programador debe declarar una instancia de Arbitrary.

-- Para nuestro caso, vamos crear una funcion generadora de listas ordenadas del tipo a.
-- Utilizamos la funcion oneof que escoge un elemento aleatoriamente de entre una lista de valores.
-- Uno de los casos es la lista vacia (return []). El otro hace uso de la recusión para generar 
-- valores aleatorios hasta que salga una lista vacia, en cuyo caso hace append de todos los valores
-- generados y ya tenemos resultado.

miOrderedList :: (Ord a, Arbitrary a) => Gen [a]
miOrderedList = oneof [ return [],
                        do
                          xs <- miOrderedList
                          n <- arbitrary
                          return ((case xs of
                                    [] -> n
                                    x:_ -> n `min` x):xs)
                      ]

-- Este generador es totalmente valido. Pero tiene a repetir valores. Si comprobamos con
-- verboseCheck podemos observar que muchos valores son la lista vacia y en caso de 
-- almacenar elementos suele repetir bastante ya que coge siempre el minimo de todos los valores
-- de la lista para asegurarse de que esta ordenado.

-- [-84, -84, -84, -14, -14]

-- Ejercicio 12
-- Otra forma de crear el generador para dar mas riqueza es usar "frequency"
-- Se genera un numero aleatorio n y creamos una funcion local que dado un numero n,
-- devuelve una lista vacia (1 de cada 5 veces) o hace uso de la recursion para generar
-- otro numero aleatorio m, le suma n y vuelve a llamarse a si mismo hasta que obtiene una lista vacia,
-- en cuyo caso hace append de todos los valores obtenidos.
-- Las listas no repiten elementos ya que siempre se suma un numero aleatorio al anterior
-- en la pila de llamadas recursivas empezando desde n.
-- De esta forma obtenemos una lista ordenada ascendentemente.

miOrderedList2 :: (Num a, Arbitrary a) => Gen [a]
miOrderedList2 = do
                    n <- arbitrary
                    listFrom n where
                                  listFrom n = frequency [(1, return []),
                                                (4, do
                                                      m <- arbitrary
                                                      ns <- listFrom (n + abs m)
                                                      return (n:ns))]
