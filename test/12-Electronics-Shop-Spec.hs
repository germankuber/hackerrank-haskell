module Spec where

import ElectronicsShop
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      6 `shouldBe` 6
         