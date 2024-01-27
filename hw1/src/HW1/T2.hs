module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import Numeric.Natural

data N = Z | S N


-- Pred:
--      There are no specific pres for this function.
-- Post:
--      The function returns the sum of two N values.
nplus :: N -> N -> N
nplus Z n = n
nplus (S m) n = S (nplus m n)


-- Pred:
--      There are no specific pres for this function.
-- Post:
--      The function returns the product of two N values.
nmult :: N -> N -> N
nmult Z _ = Z
nmult (S m) n = nplus n (nmult m n)


-- Pred:
--      There are no specific pres for this function.
-- Post:
--      The function returns the difference between two N values.
--      If the difference is defined, it returns Just with the result;
--      otherwise, it returns Nothing.
nsub :: N -> N -> Maybe N
nsub Z (S _) = Nothing
nsub Z _ = Just Z
nsub m Z = Just m
nsub (S m) (S n) = nsub m n


-- Pred:
--      There are no specific pres for this function.
-- Post:
--      The function returns the ordering between two N values.
--      It can be LT (less than), EQ (equal to), or GT (greater than).
ncmp :: N -> N -> Ordering
ncmp Z Z = EQ
ncmp Z _ = LT
ncmp _ Z = GT
ncmp (S m) (S n) = ncmp m n


-- Pred:
--      There are no specific pres for this function.
-- Post:
--      The function converts a Natural value to its equivalent N representation.
nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S (nFromNatural (n - 1))


-- Pred:
--       There are no specific pres for this function.
-- Post:
--       The function converts an N value to
--       its equivalent representation in a numeric type a (which should be an instance of the Num typeclass).
nToNum :: Num a => N -> a
nToNum Z = 0
nToNum (S n) = 1 + nToNum n


-- Pred:
--      There are no specific pres for this function.
-- Post:
--      The function returns True if the input N value is even, and False if it is odd.
nEven :: N -> Bool
nEven Z = True
nEven (S Z) = False
nEven (S (S a)) = nEven a
      

-- Pred:
--      There are no specific pres for this function.
-- Post:
--      The function returns True if the input N value is odd, and False if it is even.
nOdd :: N -> Bool
nOdd n = case nEven n of
    True -> False
    False -> True


-- Pred:
--      The second argument should not be zero.
-- Post:
--      The function returns the result of integer division between two N values.
--      If the division by zero occurs, it throws an error.
ndiv :: N -> N -> N
ndiv _ Z = error "divide by zero"
ndiv Z _ = Z
ndiv n m = case nsub n m of
    Just number -> nplus (S Z) (ndiv number m)
    Just Z -> Z
    Nothing -> Z


-- Pred: 
--      The second argument should not be zero.
-- Post: 
--      The function returns the remainder of division between two N values. 
--      If the division by zero occurs, it throws an error.
nmod :: N -> N -> N
nmod _ Z = error "mod by zero"
nmod n m = case nsub n (nmult m (ndiv n m)) of
    Just number -> number
    Nothing -> Z
