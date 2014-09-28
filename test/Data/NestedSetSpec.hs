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
        forestToNestedSets [Node 'a' []] `shouldBe` [Capacity 1 2 []]

    it "generates nested sets for two nodes" $ do
        forestToNestedSets [Node 'a' [], Node 'b' []] `shouldBe` [Capacity 1 2 [], Capacity 3 4 []]

    it "generates nested sets for nested nodes" $ do
        forestToNestedSets [Node 'a' [Node 'b' []]] `shouldBe` [Capacity 1 4 [Capacity 2 3 []]]

    it "generates nested sets for several nested nodes" $ do
        forestToNestedSets [Node 'a' [
                                Node 'b' [
                                    Node 'c' [],
                                    Node 'd' []]],
                            Node 'b' [
                                Node 'c' [],
                                Node 'd' []]]
            `shouldBe` [Capacity 1 8 [
                            Capacity 2 7 [
                                Capacity 3 4 [],
                                Capacity 5 6 []]],
                        Capacity 9 14 [
                            Capacity 10 11 [],
                            Capacity 12 13 []]]
