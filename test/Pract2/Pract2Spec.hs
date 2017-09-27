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
    quickCheck prop_Ordered
  it "Exercise 8: Cycle is equal to concatenate with parameter itself" $
    quickCheck prop_DoubleCycle

prop_RevUnit :: [Int] -> Bool
prop_RevUnit x = reverse [x] == [x]

prop_RevApp :: [Int] -> [Int] -> Bool
prop_RevApp xs ys = reverse (xs++ys) == reverse ys ++ reverse xs

prop_RevRev :: [Int] -> Bool
prop_RevRev list = reverse (reverse list) == list

prop_MaxLe :: Int -> Int -> Property
prop_MaxLe x y = x <= y ==> max x y == y

prop_Ordered :: Int -> [Int] -> Property
prop_Ordered x ys = miOrdered ys ==> collect (length ys) $ miInsert x ys == sort (x:ys)

prop_DoubleCycle :: [Int] -> Int -> Property
prop_DoubleCycle xs n = not (null xs) && n >= 0 ==> take n (cycle xs) == take n (cycle (xs ++ xs))

class Arbitrary a where
  arbitrary :: Gen a
  coarbitrary :: a -> Gen b -> Gen b