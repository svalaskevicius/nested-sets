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

import Data.Tree (Forest, Tree (..), rootLabel, subForest)

type NestedSets a = [NestedSetsNode a]
type Position = (Int, Int)

data NestedSetsNode a = NestedSetsNode {
    position :: Position,
    content  :: a,
    children :: NestedSets a
} deriving (Show, Eq)

forestToNestedSets :: Forest a -> NestedSets a
forestToNestedSets = fst . nestedSetsStartingAt ([], 0)
    where
        nestedSetsStartingAt (_, start) = foldl nestedSetsForElement ([], start)
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
nestedSetsNextSiblingPosition (first : ds) pos
    | position first == pos = firstPositionOf ds
    | isPositionParent (position first) pos = nestedSetsNextSiblingPosition (children first) pos
    | otherwise = nestedSetsNextSiblingPosition ds pos
    where firstPositionOf [] = Nothing
          firstPositionOf (firstSet : _) = Just . position $ firstSet


nestedSetsParentPosition :: NestedSets a -> Position -> Maybe Position
nestedSetsParentPosition [] _ = Nothing
nestedSetsParentPosition (firstSet:ds) pos
    | isPositionParent (position firstSet) pos = descendToChildren firstSet
    | otherwise = nestedSetsParentPosition ds pos
    where findParentPos [] _ = Nothing
          findParentPos (x : xs) currentParent
            | position x == pos = Just . position $ currentParent
            | isPositionParent (position x) pos = descendToChildren x
            | otherwise = findParentPos xs currentParent
          descendToChildren set = findParentPos (children set) set


nestedSetsFirstChildPosition :: NestedSets a -> Position -> Maybe Position
nestedSetsFirstChildPosition [] _ = Nothing
nestedSetsFirstChildPosition (first : ds) pos
    | position first == pos = firstPosition . children $ first
    | isPositionParent (position first) pos = nestedSetsFirstChildPosition (children first) pos
    | otherwise = nestedSetsFirstChildPosition ds pos
    where firstPosition [] = Nothing
          firstPosition (x : _) = Just . position $ x

nestedSetsPositionValue :: NestedSets a -> Position -> Maybe a
nestedSetsPositionValue [] _ = Nothing
nestedSetsPositionValue (first : ds) pos
    | position first == pos = Just . content $ first
    | isPositionParent (position first) pos = nestedSetsPositionValue (children first) pos
    | otherwise = nestedSetsPositionValue ds pos


isPositionParent :: Position -> Position -> Bool
isPositionParent (parentL, parentR) (childL, childR) = parentL < childL && parentR > childR

