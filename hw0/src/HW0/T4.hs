{-# LANGUAGE LambdaCase #-}

module HW0.T4
  ( fac,
    fib,
    map',
    repeat',
  )
where

import           Data.Function   (fix)
import           Numeric.Natural (Natural)

repeat' :: a -> [a]
repeat' x = fix (x :)

map' :: (a -> b) -> [a] -> [b]
map' f = fix $ \g -> \case
  [] -> []
  (h : t) -> f h : g t

fib :: Natural -> Natural
fib n = fibAccRec n 0 1

fibAccRec :: Natural -> Natural -> Natural -> Natural
fibAccRec = fix (\rec n' a' b' ->
  if n' <= 0 then a'
  else rec (n' - 1) b' (a' + b'))

fac :: Natural -> Natural
fac = fix $ \rec n -> if n == 0 then 1 else n * rec (n - 1)
