module HW0.T6
  ( a
  , a_whnf
  , b
  , b_whnf
  , c
  , c_whnf
  ) where

import HW0.T1 (distrib)
import Data.Char (isSpace)

a = distrib (Left ("AB" ++ "CD" ++ "EF"))

a_whnf = undefined

b = map isSpace "Hello, World"

b_whnf = undefined

c = if 1 > 0 || error "X" then "Y" else "Z"

c_whnf = undefined