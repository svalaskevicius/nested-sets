module Data.NestedSet (
    NestedSets(..),
    forestToNestedSets,
    ) where

import Data.Tree (Forest, subForest)

data NestedSets = NestedSets {
    left :: Int,
    right :: Int,
    children :: [NestedSets]
} deriving (Show, Eq)

forestToNestedSets :: Forest a -> [NestedSets]
forestToNestedSets = fst . nestedSetsStartingAt ([], 0)
    where
        nestedSetsStartingAt (_, start) nextForest = foldl nestedSetsForElement ([], start) nextForest
        nestedSetsForElement (siblingCapacities, start) el =
            let currentElementStart = start + 1
                (subForestNestedSets, end) = nestedSetsStartingAt ([], currentElementStart) $ subForest el
                currentElementEnd = end + 1
            in (siblingCapacities ++ [NestedSets currentElementStart currentElementEnd subForestNestedSets], currentElementEnd)
