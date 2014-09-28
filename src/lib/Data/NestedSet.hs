module Data.NestedSet (
    Capacity(..),
    forestToNestedSets,
    ) where

import Data.Tree (Forest, subForest)

data Capacity = Capacity {
    left :: Int,
    right :: Int,
    children :: [Capacity]
} deriving (Show, Eq)

forestToNestedSets :: Forest a -> [Capacity]
forestToNestedSets = fst . nestedSetsStartingAt ([], 0)
    where
        nestedSetsStartingAt (_, start) nextForest = foldl nestedSetsForElement ([], start) nextForest
        nestedSetsForElement (siblingCapacities, start) el =
            let currentElementStart = start + 1
                (subForestCapacity, end) = nestedSetsStartingAt ([], currentElementStart) $ subForest el
                currentElementEnd = end + 1
            in (siblingCapacities ++ [Capacity currentElementStart currentElementEnd subForestCapacity], currentElementEnd)
