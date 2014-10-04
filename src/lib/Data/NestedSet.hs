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
    nestedSetsPositionSetValue,
    ) where

import Data.Tree (Forest, Tree (..), rootLabel, subForest)

type NestedSets a = [NestedSetsNode a]
type Position = (Int, Int)

data NestedSetsNode a = NestedSetsNode {
    position :: Position,
    content  :: a,
    children :: NestedSets a
} deriving (Show, Eq)

-- | Convert forest to nested sets
--   This function is the opposite of 'nestedSetsToForest'
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

-- | Convert nested sets to forest.
--   This function is the opposite of 'forestToNestedSets'
nestedSetsToForest :: NestedSets a -> Forest a
nestedSetsToForest = map (\el -> Node (content el) (nestedSetsToForest $ children el))

-- | Retrieve the starting position (iterator) of the nested set.
nestedSetsStartPosition :: NestedSets a -> Maybe Position
nestedSetsStartPosition [] = Nothing
nestedSetsStartPosition (first:_) = Just . position $ first

-- | Advance the given position to the next sibling.
nestedSetsNextSiblingPosition :: NestedSets a -> Position -> Maybe Position
nestedSetsNextSiblingPosition [] _ = Nothing
nestedSetsNextSiblingPosition (first : ds) pos
    | position first == pos = firstPositionOf ds
    | isPositionParent (position first) pos = nestedSetsNextSiblingPosition (children first) pos
    | otherwise = nestedSetsNextSiblingPosition ds pos
    where firstPositionOf [] = Nothing
          firstPositionOf (firstSet : _) = Just . position $ firstSet

-- | Retrieve the position's parent position.
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

-- | Advance the position to the first child node.
nestedSetsFirstChildPosition :: NestedSets a -> Position -> Maybe Position
nestedSetsFirstChildPosition sets pos = firstPosition . (fmap children) $ findPosition sets pos
    where firstPosition Nothing = Nothing
          firstPosition (Just []) = Nothing
          firstPosition (Just (x : _)) = Just . position $ x

-- | Retrieve the value for the given 'Position'.
nestedSetsPositionValue :: NestedSets a -> Position -> Maybe a
nestedSetsPositionValue sets pos = fmap content $ findPosition sets pos

-- | Set value for the 'Position' on the given nested set.
--   Does not modify the given 'NestedSets' if the 'Position' cannot be found.
nestedSetsPositionSetValue :: NestedSets a -> Position -> a -> NestedSets a
nestedSetsPositionSetValue [] _ _ = []
nestedSetsPositionSetValue (first : ds) pos value
    | position first == pos = (first {content = value}) : ds
    | isPositionParent (position first) pos = (first {children = nestedSetsPositionSetValue (children first) pos value }) : ds
    | otherwise = first:(nestedSetsPositionSetValue ds pos value)

isPositionParent :: Position -> Position -> Bool
isPositionParent (parentL, parentR) (childL, childR) = parentL < childL && parentR > childR


findPosition :: NestedSets a -> Position -> Maybe (NestedSetsNode a)
findPosition [] _ = Nothing
findPosition (first : ds) pos
    | position first == pos = Just first
    | isPositionParent (position first) pos = findPosition (children first) pos
    | otherwise = findPosition ds pos

