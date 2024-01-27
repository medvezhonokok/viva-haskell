module HW0.T5
  ( Nat
  , nFromNatural
  , nmult
  , nplus
  , ns
  , nToNum
  , nz
  ) where

import           Numeric.Natural

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ = id

ns :: Nat a -> Nat a
ns a f = a f . f

nplus :: Nat a -> Nat a -> Nat a
nplus a b f = a f . b f

nmult :: Nat a -> Nat a -> Nat a
nmult a b = a . b

nFromNatural :: Natural -> Nat a
nFromNatural 0 = nz
nFromNatural n = ns $ nFromNatural (n - 1)

nToNum :: Num a => Nat a -> a
nToNum n = n (+ 1) 0
