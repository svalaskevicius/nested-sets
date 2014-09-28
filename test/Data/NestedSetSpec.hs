module Data.NestedSetSpec (main, spec) where

import Test.Hspec
import Data.NestedSet
import Data.Tree

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Nested set" $ do
    it "generates nested sets for empty forest" $ do
        forestToNestedSets [] `shouldBe` []

    it "generates nested sets for one node" $ do
        forestToNestedSets [Node 'a' []] `shouldBe` [NestedSets 1 2 []]

    it "generates nested sets for two nodes" $ do
        forestToNestedSets [Node 'a' [], Node 'b' []] `shouldBe` [NestedSets 1 2 [], NestedSets 3 4 []]

    it "generates nested sets for nested nodes" $ do
        forestToNestedSets [Node 'a' [Node 'b' []]] `shouldBe` [NestedSets 1 4 [NestedSets 2 3 []]]

    it "generates nested sets for several nested nodes" $ do
        forestToNestedSets complexForest `shouldBe` complexNestedSets


complexForest :: Forest Char
complexForest = [Node 'a' [
                     Node 'b' [
                         Node 'c' [],
                         Node 'd' []]],
                 Node 'b' [
                     Node 'c' [],
                     Node 'd' []]]

complexNestedSets :: [NestedSets]
complexNestedSets = [NestedSets 1 8 [
                         NestedSets 2 7 [
                             NestedSets 3 4 [],
                             NestedSets 5 6 []]],
                     NestedSets 9 14 [
                         NestedSets 10 11 [],
                         NestedSets 12 13 []]]

