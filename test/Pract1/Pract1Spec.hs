module Pract1.Pract1Spec where

import Test.Hspec
import Pract1.Pract1

spec = describe "Pract1 tests" $ do
    it "increments returns the successor of a number" $
        increment 1 `shouldBe` (2 :: Int)

    it "areEqual returns True if both elements are the same" $
        areEqual 1 1 `shouldBe` (True :: Bool)

    it "isZero returns True if element is 0" $
        isZero 0 `shouldBe` (True :: Bool)

    it "isZeroGuarded returns False if element is not 0" $
        all isZeroGuarded [-1, 1, 100, 3] `shouldBe` (False :: Bool)
        
    it "nthElement returns the nth element in a list" $
        nthElement [1,2,3,4] 3 `shouldBe` (4 :: Int)

    it "nthElement' returns the nth element in a list" $
        nthElement' [1,2,3,4] 3 `shouldBe` (4 :: Int)
        
    it "nthElement'' returns Just(nth element) in a list if found" $
        nthElement'' [1,2,3,4] 3 `shouldBe` (Just 4 :: Maybe Int)

    it "nthElement'' returns Nothing if nth position is out of bound" $
        nthElement'' [1,2,3,4] 4 `shouldBe` (Nothing :: Maybe Int)

    it "strToAscii returns vector of ASCII codes of string" $
        strToAscii "ABCxyz" `shouldBe` ([65, 66, 67, 120, 121, 122] :: [Int])
