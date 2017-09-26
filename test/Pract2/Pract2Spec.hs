module Pract2.Pract2Spec where
  
import Test.Hspec
import Test.QuickCheck

spec = describe "Pract2 tests" $ do
  it "Exercise 1.1: reverse unit is equal to unit" $
    property prop_RevUnit
  it "Exercise 1.2: reverse applied to list is equal to reversed list (Is this okay?)" $
    property prop_RevApp
  it "Exercise 1.3: reverse applied to reversed list is equal to list" $
    property prop_RevRev

prop_RevUnit :: [Int] -> Bool
prop_RevUnit [x] = reverse [x] == [x]

prop_RevApp :: [Int] -> [Int] -> Bool
prop_RevApp xs ys = reverse (xs++ys) == reverse ys ++ reverse xs

prop_RevRev :: [Int] -> Bool
prop_RevRev list = reverse (reverse list) == list