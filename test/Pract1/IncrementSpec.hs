module Pract1.IncrementSpec where

import Test.Hspec
import Pract1.Increment

spec = describe "Increment test" $ do
    it "returns the successor of a number" $ do
        increment 1 `shouldBe` (2 :: Int)
        

