module Data.NestedSetSpec (main, spec) where

import Data.NestedSet
import Data.Tree
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Nested set" $ do
    describe "forestToNestedSets" $ do
        it "generates nested sets for empty forest" $
            forestToNestedSets [] `shouldBe` ([]::NestedSets Char)

        it "generates nested sets for one node" $
            forestToNestedSets [Node 'a' []] `shouldBe` [NestedSetsNode (1, 2) 'a' []]

        it "generates nested sets for two nodes" $
            forestToNestedSets [Node 'a' [], Node 'b' []] `shouldBe` [NestedSetsNode (1, 2) 'a' [], NestedSetsNode (3, 4) 'b' []]

        it "generates nested sets for nested nodes" $
            forestToNestedSets [Node 'a' [Node 'b' []]] `shouldBe` [NestedSetsNode (1, 4) 'a' [NestedSetsNode (2, 3) 'b' []]]

        it "generates nested sets for several nested nodes" $
            forestToNestedSets complexForest `shouldBe` complexNestedSets

    describe "nestedSetsToForest" $ do
        it "generates empty forest on empty input" $
            nestedSetsToForest [] `shouldBe` ([]::Forest Char)

        it "generates forest for one node" $
            nestedSetsToForest [NestedSetsNode (1, 2) 'a' []] `shouldBe` [Node 'a' []]

        it "generates forest for two nodes" $
            nestedSetsToForest [NestedSetsNode (1, 2) 'a' [], NestedSetsNode (3, 4) 'b' []] `shouldBe` [Node 'a' [], Node 'b' []]

        it "generates forest for nested nodes" $
            nestedSetsToForest [NestedSetsNode (1, 2) 'a' [NestedSetsNode (3, 4) 'b' []]] `shouldBe` [Node 'a' [Node 'b' []]]

        it "converts nested sets to forest" $
            nestedSetsToForest complexNestedSets `shouldBe` complexForest

    describe "nestedSetsStartPosition" $ do
        it "returns Nothing on empty input" $
            nestedSetsStartPosition [] `shouldBe` Nothing

        it "returns first position on nonempty input" $ do
            nestedSetsStartPosition [NestedSetsNode (1, 2) 'a' []] `shouldBe` Just (1, 2)
            nestedSetsStartPosition complexNestedSets `shouldBe` Just (1, 8)

    describe "nestedSetsNextSiblingPosition" $ do
        it "returns Nothing on empty input" $
            nestedSetsNextSiblingPosition [] (0, 0) `shouldBe` Nothing

        it "returns Nothing if there is just one node" $ do
            nestedSetsNextSiblingPosition [NestedSetsNode (1, 2) 'a' []] (0, 0) `shouldBe` Nothing
            nestedSetsNextSiblingPosition [NestedSetsNode (1, 2) 'a' []] (1, 2) `shouldBe` Nothing

        it "returns position to the second node if the first node is the starting point" $
            nestedSetsNextSiblingPosition [NestedSetsNode (1, 2) 'a' [], NestedSetsNode (3, 4) 'b' []] (1, 2) `shouldBe` Just (3, 4)

        it "returns Nothing if position is not found" $
            nestedSetsNextSiblingPosition [NestedSetsNode (1, 2) 'a' [], NestedSetsNode (3, 4) 'b' []] (1, 1) `shouldBe` Nothing

        it "returns position to the third node if the second node is the starting point" $
            nestedSetsNextSiblingPosition [NestedSetsNode (1, 2) 'a' [], NestedSetsNode (3, 4) 'b' [], NestedSetsNode (5, 6) 'c' []] (3, 4) `shouldBe` Just (5, 6)

        it "advances a nested position" $ do
            nestedSetsNextSiblingPosition complexNestedSets (3, 4) `shouldBe` Just (5, 6)
            nestedSetsNextSiblingPosition complexNestedSets (10, 11) `shouldBe` Just (12, 13)
            nestedSetsNextSiblingPosition complexNestedSets (12, 13) `shouldBe` Nothing
            nestedSetsNextSiblingPosition complexNestedSets (2, 7) `shouldBe` Nothing

    describe "nestedSetsParentPosition" $ do
        it "returns Nothing on empty set" $
            nestedSetsParentPosition [] (0, 0) `shouldBe` Nothing

        it "returns parent position of the first level" $
            nestedSetsParentPosition complexNestedSets (2, 7) `shouldBe` Just (1, 8)

        it "returns Nothing if parent of the first level position is requested" $
            nestedSetsParentPosition [NestedSetsNode (1, 2) 'a' []] (1, 2) `shouldBe` Nothing

        it "returns Nothing if parent of the unknown position is requested" $ do
            nestedSetsParentPosition [NestedSetsNode (1, 2) 'a' []] (4, 9) `shouldBe` Nothing
            nestedSetsParentPosition complexNestedSets (4, 6) `shouldBe` Nothing

    describe "nestedSetsFirstChildPosition" $ do
        it "returns Nothing on empty set" $
            nestedSetsFirstChildPosition [] (0, 0) `shouldBe` Nothing

        it "returns child position of the first level" $ do
            nestedSetsFirstChildPosition complexNestedSets (1, 8) `shouldBe` Just (2, 7)
            nestedSetsFirstChildPosition complexNestedSets (9, 14) `shouldBe` Just (10, 11)

        it "returns child position of a deeper level" $
            nestedSetsFirstChildPosition complexNestedSets (2, 7) `shouldBe` Just (3, 4)

        it "returns Nothing if a node does not have children" $
            nestedSetsFirstChildPosition complexNestedSets (10, 11) `shouldBe` Nothing

    describe "nestedSetsPositionValue" $ do
        it "returns Nothing on empty set" $
            nestedSetsPositionValue [] (0, 0) `shouldBe` (Nothing::Maybe Char)

        it "returns value of first level position" $ do
            nestedSetsPositionValue complexNestedSets (1, 8) `shouldBe` Just 'a'
            nestedSetsPositionValue complexNestedSets (9, 14) `shouldBe` Just 'e'

        it "returns value of deeper position" $ do
            nestedSetsPositionValue complexNestedSets (3, 4) `shouldBe` Just 'c'
            nestedSetsPositionValue complexNestedSets (12, 13) `shouldBe` Just 'g'

        it "returns Nothing if it cannot find the position" $
            nestedSetsPositionValue complexNestedSets (4, 5) `shouldBe` Nothing

    describe  "nestedSetsPositionSetValue" $ do
        it "returns unmodified nested set if position is not found" $ do
            let ns = nestedSetsPositionSetValue complexNestedSets (100, 80) 'X'
            ns `shouldBe` complexNestedSets

        it "returns modified nested set" $ do
            let ns = nestedSetsPositionSetValue complexNestedSets (1, 8) 'X'
            nestedSetsPositionValue ns (1, 8) `shouldBe` Just 'X'



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


