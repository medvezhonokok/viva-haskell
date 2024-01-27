module HW4.T1
  ( EvaluationError (..),
    ExceptState (..),
    mapExceptState,
    wrapExceptState,
    joinExceptState,
    modifyExceptState,
    throwExceptState,
    eval,
  )
where

import Control.Monad (ap)
import HW4.Types

newtype ExceptState e s a = ES {runES :: s -> Except e (Annotated s a)}

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState ab es = ES (\s -> case runES es s of
  (Error err)           -> Error err
  (Success (x :# xs)) -> Success (ab x :# xs))

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES (\s -> Success (a :# s))

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState e = ES (\s -> case runES e s of
  (Error err)               -> Error err
  (Success (nEs :# ns)) -> runES nEs ns)

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES (\s -> Success (() :# f s))

throwExceptState :: e -> ExceptState e s a
throwExceptState err = ES (\_ -> Error err)

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure    = wrapExceptState
  p <*> q = ap p q

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState (fmap f m)

data EvaluationError = DivideByZero
  deriving (Show)

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val value) = pure value
eval (Op operation) = case operation of
  (Add x y) -> binOp Add (+) x y
  (Sub x y) -> binOp Sub (-) x y
  (Mul x y) -> binOp Mul (*) x y
  (Abs x)   -> unaryOp Abs abs x
  (Sgn x)   -> unaryOp Sgn signum x
  (Div x y) -> do
    x' <- eval x
    y' <- eval y
    if y' /= 0
      then do
        modifyExceptState (Div x' y' :)
        pure (x' / y')
      else throwExceptState DivideByZero

binOp :: (Double -> Double -> Prim Double) -> (Double -> Double -> Double) -> Expr -> Expr -> ExceptState EvaluationError [Prim Double] Double
binOp operation sign x y = do
  x' <- eval x
  y' <- eval y
  modifyExceptState (operation x' y' :)
  pure (sign x' y')

unaryOp :: (Double -> Prim Double) -> (Double -> Double) -> Expr -> ExceptState EvaluationError [Prim Double] Double
unaryOp operation sign x = do
  x' <- eval x
  modifyExceptState (operation x' :)
  pure (sign x')
