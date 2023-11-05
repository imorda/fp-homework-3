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

import Control.Monad (join)
import HW3.T1

newtype State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f stateA = do
   a <- stateA
   return $ f a

wrapState :: a -> State s a
wrapState a = S $ \s -> a :# s

joinState :: State s (State s a) -> State s a
joinState = join

modifyState :: (s -> s) -> State s ()
modifyState f = S $ \s -> () :# f s

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  (<*>) stateF stateA = do
     f <- stateF
     mapState f stateA

instance Monad (State s) where
  (>>=) (S runA) f = S $ \s ->
    let a :# newS = runA s in
      let S runB = f a in
        runB newS

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

eval :: Expr -> State [Prim Double] Double

eval (Val x) = return x

eval (Op (Add lhsExpr rhsExpr)) = do
  lhsVal <- eval lhsExpr
  rhsVal <- eval rhsExpr
  modifyState (Add lhsVal rhsVal :)
  return $ lhsVal + rhsVal

eval (Op (Sub lhsExpr rhsExpr)) = do
  lhsVal <- eval lhsExpr
  rhsVal <- eval rhsExpr
  modifyState (Sub lhsVal rhsVal :)
  return $ lhsVal - rhsVal

eval (Op (Mul lhsExpr rhsExpr)) = do
  lhsVal <- eval lhsExpr
  rhsVal <- eval rhsExpr
  modifyState (Mul lhsVal rhsVal :)
  return $ lhsVal * rhsVal

eval (Op (Div lhsExpr rhsExpr)) = do
  lhsVal <- eval lhsExpr
  rhsVal <- eval rhsExpr
  modifyState (Div lhsVal rhsVal :)
  return $ lhsVal / rhsVal

eval (Op (Abs expr)) = do
  val <- eval expr
  modifyState (Abs val :)
  return $ abs val

eval (Op (Sgn expr)) = do
  val <- eval expr
  modifyState (Sgn val :)
  return $ signum val
