module GtkExtras.NestedSetSpec (main, spec) where

import Test.Hspec
import GtkExtras.NestedSet

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Nested set" $ do
    it "generates capacity tree for empty forest" $ do
        calcForestCapacity [] `shouldBe` (Capacity 1 2 [])
