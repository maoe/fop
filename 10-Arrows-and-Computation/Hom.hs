{-# LANGUAGE Arrows #-}
module Hom where
import Control.Category (Category(..), (>>>))
import Control.Arrow    (Arrow(..), returnA)
import Data.Tuple       (swap)
import Prelude hiding   ((.), id)

data BalTree a = Zero a
               | Succ (BalTree (Pair a))
               deriving Show

type Pair a = (a, a)

tree0, tree1, tree2, tree3 :: BalTree Int
tree0 = Zero 1
tree1 = Succ (Zero (1, 2))
tree2 = Succ (Succ (Zero ((1, 2), (3, 4))))
tree3 = Succ (Succ (Succ (Zero (((1, 2), (3, 4)), ((5, 6), (7, 8))))))
tree4 = Succ (Succ (Succ (Zero (((8, 6), (1, 0)), ((7, 6), (9, 2))))))

data Hom a b = (a -> b) :&: Hom (Pair a) (Pair b)

apply :: Hom a b -> BalTree a -> BalTree b
apply (f :&: _)  (Zero a) = Zero (f a)
apply (_ :&: fs) (Succ t) = Succ (apply fs t)

instance Category Hom where
  id = id :&: id
  (f :&: fs) . (g :&: gs) = (f . g) :&: (fs . gs)

instance Arrow Hom where
  arr f = f :&: arr (f *** f)
  first (f :&: fs) = (f *** id) :&: (arr transpose >>> first fs >>> arr transpose)

transpose :: ((a, b), (c, d)) -> ((a, c), (b, d))
transpose ((a, b), (c, d)) = ((a, c), (b, d))


-- Parallel prefix

scan :: Num a => Hom a a
scan = id :&: proc (o, e) -> do
  e' <- scan -< o + e
  el <- rsh 0 -< e'
  returnA -< (el + o, e')

rsh :: a -> Hom a a
rsh v = const v :&: proc (o, e) -> do
  o' <- rsh v -< e
  returnA -< (o', o)

-- Buttfly circuits

butterfly :: (Pair a -> Pair a) -> Hom a a
butterfly f = id :&: proc (o, e) -> do
  o' <- butterfly f -< o
  e' <- butterfly f -< e
  returnA -< f (o', e')

rev :: Hom a a
rev = butterfly swap

unriffle :: Hom (Pair a) (Pair a)
unriffle = butterfly transpose

bisort :: Ord a => Hom a a
bisort = butterfly cmp
  where cmp (x, y) = (min x y, max x y)

sort :: Ord a => Hom a a
sort = undefined

