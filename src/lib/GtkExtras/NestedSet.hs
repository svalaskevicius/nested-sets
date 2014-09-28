
module GtkExtras.NestedSet (
    Capacity(..),
    calcForestCapacity,
    ) where

import Data.Tree (Forest, subForest)

data Capacity = Capacity {
    left :: Int,
    right :: Int,
    children :: [Capacity]
} deriving (Show, Eq)

calcForestCapacity :: Forest a -> [Capacity]
calcForestCapacity = fst . calcCapacityFrom ([], 0)
    where
        calcCapacityFrom (_, start) nextForest = foldl calcCapacityForElement ([], start) nextForest
        calcCapacityForElement (siblingCapacities, start) el =
            let currentElementStart = start + 1
                (subForestCapacity, end) = calcCapacityFrom ([], currentElementStart) $ subForest el
                currentElementEnd = end + 1
            in (siblingCapacities ++ [Capacity currentElementStart currentElementEnd subForestCapacity], currentElementEnd)
