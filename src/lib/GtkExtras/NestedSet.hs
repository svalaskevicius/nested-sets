
module GtkExtras.NestedSet (
    Capacity(..),
    calcForestCapacity,
    ) where

import Data.Tree (Forest)

data Capacity = Capacity {
    left :: Int,
    right :: Int,
    children :: [Capacity]
} deriving (Show, Eq)

calcForestCapacity :: Forest a -> Capacity
calcForestCapacity _ = Capacity 1 2 []
