module HW3.T2
  ( distOption
  , wrapOption
  , distPair
  , wrapPair
  , distQuad
  , wrapQuad
  , distAnnotated
  , wrapAnnotated
  , distExcept
  , wrapExcept
  , distPrioritised
  , wrapPrioritised
  , distStream
  , wrapStream
  , distList
  , wrapList
  , distFun
  , wrapFun
  ) where

import HW3.T1
import Prelude (Monoid (mempty), Semigroup ((<>)), ($))

distOption :: (Option a, Option b) -> Option (a, b)
distOption (None, _)        =  None
distOption (_, None)        =  None
distOption (Some a, Some b) = Some (a, b)

wrapOption :: a -> Option a
wrapOption = Some

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P al ar, P bl br) = P (al, bl) (ar, br)

wrapPair :: a -> Pair a
wrapPair x = P x x

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q la lb lc ld, Q ra rb rc rd) = Q (la, ra) (lb, rb) (lc, rc) (ld, rd)

wrapQuad :: a -> Quad a
wrapQuad x = Q x x x x

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (la :# le, ra :# re) = (la, ra) :# le <> re

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated x = x :# mempty

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error e, _)           = Error e
distExcept (_, Error e)           = Error e
distExcept (Success a, Success b) = Success (a, b)

wrapExcept :: a -> Except e a
wrapExcept = Success

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (Low a, Low b)       = Low (a, b)
distPrioritised (Low a, Medium b)    = Medium (a, b)
distPrioritised (Low a, High b)      = High (a, b)
distPrioritised (Medium a, Low b)    = Medium (a, b)
distPrioritised (Medium a, Medium b) = Medium (a, b)
distPrioritised (Medium a, High b)   = High (a, b)
distPrioritised (High a, Low b)      = High (a, b)
distPrioritised (High a, Medium b)   = High (a, b)
distPrioritised (High a, High b)     = High (a, b)

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (x :> xs, y :> ys) = (x, y) :> distStream (xs, ys)

wrapStream :: a -> Stream a
wrapStream x = x :> wrapStream x

distList :: (List a, List b) -> List (a, b)
distList (Nil, _)     = Nil
distList (_, Nil)     = Nil
distList (x :. xs, y) = mapList (\yi -> (x, yi)) y ++ distList (xs, y)

wrapList :: a -> List a
wrapList x = x :. Nil

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F x, F y) = F $ \i -> (x i, y i)

wrapFun :: a -> Fun i a
wrapFun x = F $ \_ -> x
