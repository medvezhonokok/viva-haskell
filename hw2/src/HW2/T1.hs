module HW2.T1
  ( Tree (..)
  , tfoldr
  , treeToList
  ) where

data Tree a = Leaf | Branch !Int (Tree a) a (Tree a)
  deriving (Show)

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ b Leaf = b
tfoldr f b (Branch _ left node right) = tfoldr f (f node (tfoldr f b right)) left

treeToList :: Tree a -> [a]
treeToList = tfoldr (:) []

