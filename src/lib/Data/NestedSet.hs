module Data.NestedSet (
    NestedSets,
    Position,
    NestedSetsNode(..),
    forestToNestedSets,
    nestedSetsToForest,
    nestedSetsStartPosition,
    ) where

import Data.Tree (Forest, subForest, rootLabel, Tree(..))

type NestedSets a = [NestedSetsNode a]
type Position = (Int, Int)

data NestedSetsNode a = NestedSetsNode {
    position :: Position,
    content :: a,
    children :: NestedSets a
} deriving (Show, Eq)

forestToNestedSets :: Forest a -> NestedSets a
forestToNestedSets = fst . nestedSetsStartingAt ([], 0)
    where
        nestedSetsStartingAt (_, start) nextForest = foldl nestedSetsForElement ([], start) nextForest
        nestedSetsForElement (siblingCapacities, start) el =
            let currentElementStart = start + 1
                (subForestNestedSets, end) = nestedSetsStartingAt ([], currentElementStart) $ subForest el
                currentElementEnd = end + 1
                elementContent = rootLabel el
            in (siblingCapacities ++ [NestedSetsNode (currentElementStart, currentElementEnd) elementContent subForestNestedSets], currentElementEnd)

nestedSetsToForest :: NestedSets a -> Forest a
nestedSetsToForest = map (\el -> Node (content el) (nestedSetsToForest $ children el))

nestedSetsStartPosition :: NestedSets a -> Maybe Position
nestedSetsStartPosition [] = Nothing
nestedSetsStartPosition (first:_) = Just . position $ first
