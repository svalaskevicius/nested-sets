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

    describe "nestedSetsStartPosition" $ do
        it "returns Nothing on empty input" $ do
            nestedSetsStartPosition [] `shouldBe` Nothing

        it "returns first position on nonempty input" $ do
            nestedSetsStartPosition [NestedSetsNode (1, 2) 'a' []] `shouldBe` (Just (1, 2))
            nestedSetsStartPosition complexNestedSets `shouldBe` (Just (1, 8))

    describe "nestedSetsNextSiblingPosition" $ do
        it "returns Nothing on empty input" $ do
            nestedSetsNextSiblingPosition [] (0, 0) `shouldBe` Nothing

        it "returns Nothing if there is just one node" $ do
            nestedSetsNextSiblingPosition [NestedSetsNode (1, 2) 'a' []] (0, 0) `shouldBe` Nothing
            nestedSetsNextSiblingPosition [NestedSetsNode (1, 2) 'a' []] (1, 2) `shouldBe` Nothing

        it "returns position to the second node if the first node is the starting point" $ do
            nestedSetsNextSiblingPosition [NestedSetsNode (1, 2) 'a' [], NestedSetsNode (3, 4) 'b' []] (1, 2) `shouldBe` (Just (3, 4))

        it "returns Nothing if position is not found" $ do
            nestedSetsNextSiblingPosition [NestedSetsNode (1, 2) 'a' [], NestedSetsNode (3, 4) 'b' []] (1, 1) `shouldBe` Nothing

        it "returns position to the third node if the second node is the starting point" $ do
            nestedSetsNextSiblingPosition [NestedSetsNode (1, 2) 'a' [], NestedSetsNode (3, 4) 'b' [], NestedSetsNode (5, 6) 'c' []] (3, 4) `shouldBe` (Just (5, 6))

        it "advances a nested position" $ do
            nestedSetsNextSiblingPosition complexNestedSets (3, 4) `shouldBe` (Just (5, 6))
            nestedSetsNextSiblingPosition complexNestedSets (10, 11) `shouldBe` (Just (12, 13))
            nestedSetsNextSiblingPosition complexNestedSets (12, 13) `shouldBe` Nothing
            nestedSetsNextSiblingPosition complexNestedSets (2, 7) `shouldBe` Nothing

    describe "nestedSetsParentPosition" $ do
        it "returns Nothing on empty set" $ do
            nestedSetsParentPosition [] (0, 0) `shouldBe` Nothing

        it "returns parent position of the first level" $ do
            nestedSetsParentPosition complexNestedSets (2, 7) `shouldBe` (Just (1, 8))

        it "returns Nothing if parent of the first level position is requested" $ do
            nestedSetsParentPosition [NestedSetsNode (1, 2) 'a' []] (1, 2) `shouldBe` Nothing

        it "returns Nothing if parent of the unknown position is requested" $ do
            nestedSetsParentPosition [NestedSetsNode (1, 2) 'a' []] (4, 9) `shouldBe` Nothing
            nestedSetsParentPosition complexNestedSets (4, 6) `shouldBe` Nothing

    describe "nestedSetsFirstChildPosition" $ do
        it "returns Nothing on empty set" $ do
            nestedSetsFirstChildPosition [] (0, 0) `shouldBe` Nothing

        it "returns child position of the first level" $ do
            nestedSetsFirstChildPosition complexNestedSets (1, 8) `shouldBe` (Just (2, 7))
            nestedSetsFirstChildPosition complexNestedSets (9, 14) `shouldBe` (Just (10, 11))



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


