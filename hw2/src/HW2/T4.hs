module HW2.T4
  ( DotString (..),
    Fun (..),
    Inclusive (..),
    ListPlus (..),
  )
where

data ListPlus a = a :+ ListPlus a | Last a deriving (Show)

infixr 5 :+

instance Semigroup (ListPlus a) where
  (<>) a b = case a of
    Last x -> x :+ b
    x :+ xs -> x :+ (xs <> b)

data Inclusive a b = This a | That b | Both a b

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (<>) a b = case (a, b) of
    (This x, This y) -> This (x <> y)
    (That x, That y) -> That (x <> y)
    (This x, That y) -> Both x y
    (That x, This y) -> Both y x
    (This x, Both y z) -> Both (x <> y) z
    (That x, Both y z) -> Both y (x <> z)
    (Both x y, This z) -> Both (x <> z) y
    (Both x y, That z) -> Both x (y <> z)
    (Both x y, Both z w) -> Both (x <> z) (y <> w)

newtype DotString = DS String deriving (Show)

instance Semigroup DotString where
  DS a <> DS "" = DS a
  DS "" <> DS b = DS b
  DS a <> DS b = DS (a ++ "." ++ b)

instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  F f <> F g = F (f . g)

instance Monoid (Fun a) where
  mempty = F id
