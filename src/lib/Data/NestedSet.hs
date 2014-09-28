module Data.NestedSet (
    NestedSets,
    Position,
    NestedSetsNode(..),
    forestToNestedSets,
    nestedSetsToForest,
    nestedSetsStartPosition,
    nestedSetsNextSiblingPosition,
    nestedSetsParentPosition,
    nestedSetsFirstChildPosition,
    nestedSetsPositionValue,
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

nestedSetsNextSiblingPosition :: NestedSets a -> Position -> Maybe Position
nestedSetsNextSiblingPosition [] _ = Nothing
nestedSetsNextSiblingPosition (first:ds) pos = if (position first == pos) then firstPositionOf ds
                                               else if isPositionParent (position first) pos then nestedSetsNextSiblingPosition (children first) pos
                                               else nestedSetsNextSiblingPosition ds pos
    where firstPositionOf [] = Nothing
          firstPositionOf (firstSet:_) = Just . position $ firstSet

nestedSetsParentPosition :: NestedSets a -> Position -> Maybe Position
nestedSetsParentPosition [] _ = Nothing
nestedSetsParentPosition (firstSet:ds) pos = if isPositionParent (position firstSet) pos then descendToChildren firstSet
                                             else nestedSetsParentPosition ds pos
    where findParentPos [] _ = Nothing
          findParentPos (x:xs) currentParent = if (position x == pos) then Just . position $ currentParent
                                               else if isPositionParent (position x) pos then descendToChildren x
                                               else findParentPos xs currentParent
          descendToChildren set = findParentPos (children set) set


nestedSetsFirstChildPosition :: NestedSets a -> Position -> Maybe Position
nestedSetsFirstChildPosition [] _ = Nothing
nestedSetsFirstChildPosition (first:ds) pos = if position first == pos then firstPosition . children $ first
                                              else if isPositionParent (position first) pos then nestedSetsFirstChildPosition (children first) pos
                                              else nestedSetsFirstChildPosition ds pos
    where firstPosition [] = Nothing
          firstPosition (x:_) = Just . position $ x

nestedSetsPositionValue :: NestedSets a -> Position -> Maybe a
nestedSetsPositionValue [] _ = Nothing
nestedSetsPositionValue (first:ds) pos = if position first == pos then Just . content $ first
                                         else if isPositionParent (position first) pos then nestedSetsPositionValue (children first) pos
                                         else nestedSetsPositionValue ds pos

isPositionParent :: Position -> Position -> Bool
isPositionParent (parentL, parentR) (childL, childR) = parentL < childL && parentR > childR

