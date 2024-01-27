module HW3.T1
  ( Option (..),
    Pair (..),
    Quad (..),
    Annotated (..),
    Except (..),
    Prioritised (..),
    Stream (..),
    List (..),
    Fun (..),
    Tree (..),
    mapOption,
    mapPair,
    mapQuad,
    mapAnnotated,
    mapExcept,
    mapPrioritised,
    mapStream,
    mapList,
    mapFun,
    mapTree,
  )
where

data Option a = None | Some a

data Pair a = P a a

data Quad a = Q a a a a

data Annotated e a = a :# e

infix 0 :#

data Except e a = Error e | Success a

data Prioritised a = Low a | Medium a | High a

data Stream a = a :> Stream a

infixr 5 :>

data List a = Nil | a :. List a

infixr 5 :.

newtype Fun i a = F (i -> a)

data Tree a = Leaf | Branch (Tree a) a (Tree a)

mapOption :: (a -> b) -> (Option a -> Option b)
mapOption _ None = None
mapOption f (Some v) = Some (f v)

mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair f (P a b) = P (f a) (f b)

mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad f (Q a b c d) = Q (f a) (f b) (f c) (f d)

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (v :# a) = f v :# a

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept _ (Error err) = Error err
mapExcept f (Success s) = Success (f s)

mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised f (Low v) = Low (f v)
mapPrioritised f (Medium v) = Medium (f v)
mapPrioritised f (High v) = High (f v)

mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream f (h :> t) = f h :> mapStream f t

mapList :: (a -> b) -> (List a -> List b)
mapList _ Nil = Nil
mapList f (h :. t) = f h :. mapList f t

mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun f (F function) = F (f . function)

mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree _ Leaf = Leaf
mapTree f (Branch l v r) = Branch (mapTree f l) (f v) (mapTree f r)