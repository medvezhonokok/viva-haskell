module HW2.T3
  ( epart
  , mcat
  ) where

import Data.Foldable
import Data.Maybe
import Data.Monoid

mcat :: Monoid a => [Maybe a] -> a
mcat = foldr (maybe id mappend) mempty

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldr combine (mempty, mempty)
  where
    combine (Left l) (lefts, rights) = (mappend l lefts, rights)
    combine (Right r) (lefts, rights) = (lefts, mappend r rights)