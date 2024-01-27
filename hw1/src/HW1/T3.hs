module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

type Meta = Int

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)


-- Pred:
--      There are no specific pres for this function.
-- Post:
--      The function returns the number of elements in the Tree. Complexity: O(1)
tsize :: Tree a -> Int
tsize Leaf = 0
tsize (Branch tSize _ _ _) = tSize


-- Pred:
--      There are no specific pres for this function.
-- Post:
--      The function returns the depth of the Tree, which is the maximum number of levels in the tree. Complexity: O(log n)
tdepth :: Tree a -> Int
tdepth Leaf = 0
tdepth (Branch _ left _ right) = 1 + max (tdepth left)  (tdepth right)


-- Pred:
--      The Tree should be ordered (`Ord`) to perform comparisons.
-- Post:
--      The function returns True if the given element is a member of the Tree, otherwise it returns False. Complexity: O(log n)
tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember key (Branch _ left node_key right) = do
     if key == node_key then True
     else if key < node_key then tmember key left
     else tmember key right


-- Pred:
--      The Tree should be ordered (`Ord`) to maintain the sorting property.
-- Post:
--      The function inserts the given element into the Tree while preserving the order of the elements.
--      It returns a new Tree with the element inserted. Complexity: O(log n)
tinsert :: Ord a => a -> Tree a -> Tree a
tinsert x Leaf = Branch 1 Leaf x Leaf
tinsert x (Branch size l y r) = 
    if x == y then Branch size l x r
    else if x < y then Branch (size + 1) (tinsert x l) y r
    else Branch (size + 1) l y (tinsert x r)


-- Pred:
--      The list should be ordered (`Ord`) to maintain the sorting property when creating the Tree.
-- Post:
--      The function creates a new Tree from a list of elements, with the elements inserted in the order they appear in the list.
--      The resulting Tree will be ordered and contain all the elements from the list. Complexity: O(n log n)
tFromList :: Ord a => [a] -> Tree a
tFromList = foldl (flip tinsert) Leaf
