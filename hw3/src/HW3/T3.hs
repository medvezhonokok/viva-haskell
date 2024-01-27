module HW3.T3
  ( joinOption,
    joinExcept,
    joinAnnotated,
    joinList,
    joinFun,
  )
where

import HW3.T1

joinOption :: Option (Option a) -> Option a
joinOption (Some x) = x
joinOption _ = None

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error e) = Error e
joinExcept (Success s) = s

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# first) :# second) = a :# (second <> first)

joinList :: List (List a) -> List a
joinList Nil = Nil
joinList (Nil :. t) = joinList t
joinList ((h :. ht) :. t) = h :. joinList (ht :. t)

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun f = F (\i -> unwrap (unwrap f i) i)
  where
    unwrap (F fun) = fun