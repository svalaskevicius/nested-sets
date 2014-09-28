module Data.NestedSetSpec (main, spec) where

import Test.Hspec
import Data.NestedSet
import Data.Tree

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Nested set" $ do
    describe "forestToNestedSets" $ do
        it "generates nested sets for empty forest" $ do
            forestToNestedSets [] `shouldBe` ([]::NestedSets Char)

        it "generates nested sets for one node" $ do
            forestToNestedSets [Node 'a' []] `shouldBe` [NestedSetsNode (1, 2) 'a' []]

        it "generates nested sets for two nodes" $ do
            forestToNestedSets [Node 'a' [], Node 'b' []] `shouldBe` [NestedSetsNode (1, 2) 'a' [], NestedSetsNode (3, 4) 'b' []]

        it "generates nested sets for nested nodes" $ do
            forestToNestedSets [Node 'a' [Node 'b' []]] `shouldBe` [NestedSetsNode (1, 4) 'a' [NestedSetsNode (2, 3) 'b' []]]

        it "generates nested sets for several nested nodes" $ do
            forestToNestedSets complexForest `shouldBe` complexNestedSets

    describe "nestedSetsToForest" $ do
        it "generates empty forest on empty input" $ do
            nestedSetsToForest [] `shouldBe` ([]::Forest Char)

        it "generates forest for one node" $ do
            nestedSetsToForest [NestedSetsNode (1, 2) 'a' []] `shouldBe` [Node 'a' []]

        it "generates forest for two nodes" $ do
            nestedSetsToForest [NestedSetsNode (1, 2) 'a' [], NestedSetsNode (3, 4) 'b' []] `shouldBe` [Node 'a' [], Node 'b' []]

        it "generates forest for nested nodes" $ do
            nestedSetsToForest [NestedSetsNode (1, 2) 'a' [NestedSetsNode (3, 4) 'b' []]] `shouldBe` [Node 'a' [Node 'b' []]]

        it "converts nested sets to forest" $ do
            nestedSetsToForest complexNestedSets `shouldBe` complexForest

    describe "nested set position" $ do
        it "retrieves first position" $ do
            nestedSetsStartPosition [] `shouldBe` Nothing
            nestedSetsStartPosition [NestedSetsNode (1, 2) 'a' []] `shouldBe` (Just (1, 2))
            nestedSetsStartPosition complexNestedSets `shouldBe` (Just (1, 8))

complexForest :: Forest Char
complexForest = [Node 'a' [
                     Node 'b' [
                         Node 'c' [],
                         Node 'd' []]],
                 Node 'e' [
                     Node 'f' [],
                     Node 'g' []]]

complexNestedSets :: NestedSets Char
complexNestedSets = [NestedSetsNode (1, 8) 'a' [
                         NestedSetsNode (2, 7) 'b' [
                             NestedSetsNode (3, 4) 'c' [],
                             NestedSetsNode (5, 6) 'd' []]],
                     NestedSetsNode (9, 14) 'e' [
                         NestedSetsNode (10, 11) 'f' [],
                         NestedSetsNode (12, 13) 'g' []]]


