module HW3.T4
  ( State (..)
  , Prim (..)
  , Expr (..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , eval
  ) where

import Control.Monad (ap)
import HW3.T1
newtype State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f (S runA) = S $ \s ->
  let a :# newS = runA s in
    f a :# newS

wrapState :: a -> State s a
wrapState a = S $ \s -> a :# s

joinState :: State s (State s a) -> State s a
joinState (S runOuter) = S $ \s ->
  let (S runInner) :# outerS = runOuter s in
    runInner outerS

modifyState :: (s -> s) -> State s ()
modifyState f = S $ \s -> () :# f s

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  (<*>) = Control.Monad.ap

instance Monad (State s) where
  (>>=) stateA f = joinState $ mapState f stateA

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving Show

data Expr = Val Double | Op (Prim Expr)
  deriving Show

instance Num Expr where
  x + y = Op $ Add x y
  x - y = Op $ Sub x y
  x * y = Op $ Mul x y
  abs x = Op $ Abs x
  signum x = Op $ Sgn x
  fromInteger x = Val $ fromInteger x

instance Fractional Expr where
  x / y = Op $ Div x y
  fromRational x = Val $ fromRational x

evalBinary :: (Double -> Double -> Double) ->
              (Double -> Double -> Prim Double) ->
              Expr -> Expr -> State [Prim Double] Double
evalBinary evalF logF lhsExpr rhsExpr = do
  lhsVal <- eval lhsExpr
  rhsVal <- eval rhsExpr
  modifyState (logF lhsVal rhsVal :)
  return $ evalF lhsVal rhsVal

evalUnary :: (Double -> Double) ->
              (Double -> Prim Double) ->
              Expr -> State [Prim Double] Double
evalUnary evalF logF expr = do
  val <- eval expr
  modifyState (logF val :)
  return $ evalF val

eval :: Expr -> State [Prim Double] Double
eval (Val x)                    = return x
eval (Op (Add lhsExpr rhsExpr)) = evalBinary (+) Add lhsExpr rhsExpr
eval (Op (Sub lhsExpr rhsExpr)) = evalBinary (-) Sub lhsExpr rhsExpr
eval (Op (Mul lhsExpr rhsExpr)) = evalBinary (*) Mul lhsExpr rhsExpr
eval (Op (Div lhsExpr rhsExpr)) = evalBinary (/) Div lhsExpr rhsExpr
eval (Op (Abs expr))            = evalUnary abs Abs expr
eval (Op (Sgn expr))            = evalUnary signum Sgn expr
