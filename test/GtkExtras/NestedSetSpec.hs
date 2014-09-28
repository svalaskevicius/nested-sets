module GtkExtras.NestedSetSpec (main, spec) where

import Test.Hspec
import GtkExtras.NestedSet
import Data.Tree

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Nested set" $ do
    it "generates capacity tree for empty forest" $ do
        calcForestCapacity [] `shouldBe` []

    it "generates capacity tree for one node" $ do
        calcForestCapacity [Node 'a' []] `shouldBe` [Capacity 1 2 []]

    it "generates capacity tree for two nodes" $ do
        calcForestCapacity [Node 'a' [], Node 'b' []] `shouldBe` [Capacity 1 2 [], Capacity 3 4 []]

    it "generates capacity tree for nested nodes" $ do
        calcForestCapacity [Node 'a' [Node 'b' []]] `shouldBe` [Capacity 1 4 [Capacity 2 3 []]]

    it "generates capacity tree for several nested nodes" $ do
        calcForestCapacity [Node 'a' [
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
