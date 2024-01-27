module HW3.T2
  ( distOption,
    wrapOption,
    distPair,
    wrapPair,
    distQuad,
    wrapQuad,
    distAnnotated,
    wrapAnnotated,
    distExcept,
    wrapExcept,
    distPrioritised,
    wrapPrioritised,
    distStream,
    wrapStream,
    distList,
    wrapList,
    distFun,
    wrapFun,
  )
where

import HW3.T1

distOption :: (Option a, Option b) -> Option (a, b)
distOption (Some first, Some second) = Some (first, second)
distOption _ = None

wrapOption :: a -> Option a
wrapOption = Some

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P a1 b1, P a2 b2) = P (a1, a2) (b1, b2)

wrapPair :: a -> Pair a
wrapPair x = P x x

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q a1 b1 c1 d1, Q a2 b2 c2 d2) = Q (a1, a2) (b1, b2) (c1, c2) (d1, d2)

wrapQuad :: a -> Quad a
wrapQuad x = Q x x x x

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (x :# first, y :# second) = (x, y) :# first <> second

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated x = x :# mempty

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Success a, Success b) = Success (a, b)
distExcept (Error err, _) = Error err
distExcept (_, Error err) = Error err

wrapExcept :: a -> Except e a
wrapExcept = Success

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (Low first, Low second) = Low (first, second)
distPrioritised (Low first, Medium second) = Medium (first, second)
distPrioritised (Low first, High second) = High (first, second)
distPrioritised (Medium first, Low second) = Medium (first, second)
distPrioritised (Medium first, Medium second) = Medium (first, second)
distPrioritised (Medium first, High second) = High (first, second)
distPrioritised (High first, Low second) = High (first, second)
distPrioritised (High first, Medium second) = High (first, second)
distPrioritised (High first, High second) = High (first, second)

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (h1 :> t1, h2 :> t2) = (h1, h2) :> distStream (t1, t2)

wrapStream :: a -> Stream a
wrapStream x = x :> wrapStream x

distList :: (List a, List b) -> List (a, b)
distList (Nil, _) = Nil
distList (_, Nil) = Nil
distList (h :. t, z) = toList (makePairs h z) (distList (t, z))
  where
    toList :: List x -> List x -> List x
    toList Nil l = l
    toList (h' :. t') l = h' :. toList t' l

    makePairs :: x -> List l -> List (x, l)
    makePairs _ Nil = Nil
    makePairs x (h' :. t') = (x, h') :. makePairs x t'

wrapList :: a -> List a
wrapList x = x :. Nil

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F g, F h) = F (\x -> (g x, h x))

wrapFun :: a -> Fun i a
wrapFun x = F (const x)
