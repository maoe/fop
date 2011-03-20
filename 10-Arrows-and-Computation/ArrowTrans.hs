{-# LANGUAGE Arrows, TypeOperators #-}
module ArrowTrans where
import Data.Tuple       (swap)
import Control.Category (Category(..), (>>>))
import Control.Arrow    (Arrow(..), ArrowChoice(..), returnA)
import Prelude hiding   (id, (.))
import Arrows           (assoc, unassoc)

newtype StateT s (~>) i o = ST ((s, i) ~> (s, o))

instance Category a => Category (StateT s a) where
  id = ST id
  ST f . ST g = ST $ f . g

instance Arrow a => Arrow (StateT s a) where
  arr f = ST (proc (s, b) -> returnA -< (s, f b))
  first (ST f) = ST $ proc (s, (b, d)) -> do
                        (s', c) <- f -< (s, b)
                        returnA -< (s', (c, d))

type State s = StateT s (->)

data Count a i o = Count !Int (a i o)

instance Category a => Category (Count a) where
  id = Count 0 id
  Count m f . Count n g = Count (m + n) (f . g)

instance Arrow a => Arrow (Count a) where
  arr f = Count 0 (arr f)
  first (Count n f) = Count n (first f)

instance ArrowChoice a => ArrowChoice (Count a) where
  left (Count n f) = Count n (left f)

-- Exercise 10.18
arrST :: Arrow a => (i -> o) -> StateT s a i o
arrST = ST . arr . second

firstST :: Arrow a => StateT s a b c -> StateT s a (b, d) (c, d)
firstST (ST f) = ST $ arr assoc . first f . arr unassoc

-- Exercise 10.19
newtype AutoFunctor (~>) i o = A (i ~> (o, AutoFunctor (~>) i o))

type Auto i o = AutoFunctor (->)

instance Arrow a => Category (AutoFunctor a) where
  id = A $ arr $ id &&& const id
  A f . A g = A $ g >>> first f >>> arr assoc >>> second (arr (uncurry (.)))

idA :: Arrow a => AutoFunctor a b b
idA = A $ proc b -> returnA -< (b, idA)

composeA :: Arrow a => AutoFunctor a c d -> AutoFunctor a b c -> AutoFunctor a b d
composeA (A f) (A g) = A $ proc b -> do
  (c, A g') <- g -< b
  (d, A f') <- f -< c
  returnA -< (d, A f' . A g')

instance Arrow a => Arrow (AutoFunctor a) where
  arr f = A $ arr $ ($) f &&& const (arr f)
  first (A f) = A $ first f >>>
                    arr assoc >>>
                    second (arr swap) >>>
                    arr unassoc >>>
                    second (arr first)

arrA :: Arrow a => (b -> c) -> AutoFunctor a b c
arrA f = A $ proc b -> returnA -< (f b, arrA f)

firstA :: Arrow a => AutoFunctor a b c -> AutoFunctor a (b, d) (c, d)
firstA (A f) = A $ proc (b, d) -> do
  (c, f') <- f -< b
  returnA -< ((c, d), firstA f')
