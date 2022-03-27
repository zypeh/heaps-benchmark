{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}

module Z.Data.Heap
  ( -- * Pairing heap data structure
    PairingHeap(..)
    -- * Construction
  , empty, singleton, fromList
    -- * Operation
  , insert, merge, toList
    -- * Query
  , isEmpty, findMin
  ) where

import qualified Z.Data.Vector as Vector

data PairingHeap a = Empty | Node a (Vector.Vector (PairingHeap a))
  deriving Show

empty :: PairingHeap a
empty = Empty

-- | /O(1)/. Create a singleton PairingHeap
singleton :: a -> PairingHeap a
singleton x = Node x Vector.empty
{-# INLINE singleton #-}

insert :: Ord a => PairingHeap a -> a -> PairingHeap a
insert heap item = merge (singleton item) heap

-- | /O(1)/. Merging two pairing heaps. The smaller root remains
-- the toor of the result, the larger element and its subtrees is
-- appended as a child of this root.
merge :: Ord a => PairingHeap a -> PairingHeap a -> PairingHeap a
merge heap Empty = heap
merge Empty heap = heap
merge heap1@(Node root1 child1) heap2@(Node root2 child2)
  -- it is because the root is the smallest in the tree
  -- put the smallest root to the newly created subtree
  -- and append the children
  | root1 < root2 = Node root1 (Vector.cons heap2 child1)
  | otherwise     = Node root2 (Vector.cons heap1 child2)

isEmpty :: PairingHeap a -> Bool
isEmpty Empty = True
isEmpty _ = False

-- | /O(1)/. Simple return the top element of the heap.
findMin :: PairingHeap a -> Maybe a
findMin Empty = Nothing
findMin (Node a _) = Just a

extractMin :: Ord a => PairingHeap a -> Maybe (a, PairingHeap a)
extractMin Empty = Nothing
extractMin (Node root children) = Just (root, mergeSub children)
  where
    mergeSub :: Ord a => Vector.Vector (PairingHeap a) -> PairingHeap a
    mergeSub = Vector.foldl' merge empty

toList :: Ord a => PairingHeap a -> [a]
toList heap = case extractMin heap of
  Nothing -> []
  Just (root, heap') -> root : toList heap'

fromList :: Ord a => [a] -> PairingHeap a
fromList = foldl insert Empty
