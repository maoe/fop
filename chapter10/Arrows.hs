{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
module Main where
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad ((>=>))
import Data.Monoid
import Prelude hiding (id, (.))

main :: IO ()
main = putStrLn "-- testST --" >> testST >>
       putStrLn "-- testND --" >> testND >>
       putStrLn "-- testMT --" >> testMT >>
       putStrLn "-- testA --"  >> testA

-- State transformers
newtype State s i o = ST { runST :: (s, i) -> (s, o) }

instance Category (State s) where
  id          = ST id
  ST f . ST g = ST $ f . g

instance Arrow (State s) where
  arr f         = ST $ second f
  first (ST f)  = ST $ assoc . first f . unassoc

assoc :: ((a, b), c) -> (a, (b, c))
assoc ~(~(a, b), c) = (a, (b, c))

unassoc :: (a, (b, c)) -> ((a, b), c)
unassoc ~(a, ~(b, c)) = ((a, b), c)

testST :: IO ()
testST = do
  let printST s = print $ runST s ("state", 0)
  printST id
  printST $ id . id
  printST $ id >>> id
  printST $ arr succ
  printST $ ST (first reverse) >>> ST (second succ)
  printST $ ST (reverse *** succ)
  printST $ ST (reverse *** succ) >>> ST (reverse *** succ)
  printST $ arr succ &&& arr succ
  printST $ arr succ `addA` arr succ

-- Nondeterminism
newtype NonDet i o = ND { runND :: i -> [o] }

instance Category NonDet where
  id          = ND pure
  ND f . ND g = ND $ g >=> f

instance Arrow NonDet where
  arr f         = ND $ pure . f
  first  (ND f) = ND $ \(i, i') -> [(o, i') | o <- f i]
  second (ND f) = ND $ \(i, i') -> [(i, o)  | o <- f i']

testND :: IO ()
testND = do
  let printND n = print $  runND n 0
  printND id
  printND $ arr succ
  printND $ arr succ >>> ND (\x -> [x .. x+5])
  printND $ arr succ >>> ND (\x -> [x .. x+5]) >>> arr pred
  printND $ ND (\x -> [x .. x+5]) &&& arr succ
  printND $ arr succ &&& arr succ
  printND $ arr succ `addA` arr succ

-- Map transformers
newtype MapTrans s i o = MT { runMT :: (s -> i) -> s -> o }

instance Category (MapTrans s) where
  id          = MT id
  MT f . MT g = MT (f . g)

instance Arrow (MapTrans s) where
  arr f         = MT (f .)
  first (MT f)  = MT $ zipMap . first f . unzipMap
  second (MT f) = MT $ zipMap . second f . unzipMap

zipMap :: (s -> a, s -> b) -> s -> (a, b)
zipMap h s = (fst h s, snd h s)

unzipMap :: (s -> (a, b)) -> (s -> a, s -> b)
unzipMap h = (fst . h, snd . h)
             
testMT :: IO ()
testMT = print $ runMT (arr succ) pred 0

-- Simple automa
newtype Auto i o = A { runA :: i -> (o, Auto i o) }

instance Category Auto where
  id        = A $ \i -> (i, id)
  A f . A g = A $ \i -> let (o, g')  = g i
                            (o', f') = f o
                        in (o', f' . g')

instance Arrow Auto where
  arr f    = A $ \i -> (f i, arr f)
  first (A f)  = A $ \(i, i') -> let (o, f') = f i
                                 in ((o, i'), first f')
  second (A f) = A $ \(i, i') -> let (o, f') = f i'
                                 in ((i, o), second f')

evalA :: Auto i o -> i -> o
evalA a = fst . runA a

testA :: IO ()
testA = undefined

-- commons
addA :: Arrow (~>) => a ~> Int -> a ~> Int -> a ~> Int
addA f g = f &&& g >>> arr (uncurry (+))


-- exercise 10.1
newtype Reader r i o = R ((r, i) -> o)

instance Category (Reader r) where
  id        = R snd
  R f . R g = R $ uncurry $ \r -> curry f r . curry g r

instance Arrow (Reader r) where
  arr f = R $ uncurry $ const f
  first f = undefined

newtype Monoid m => Writer m i o = W (i -> (m, o))

instance Monoid m => Category (Writer m) where
  id        = W (mempty,)
  W f . W g = undefined
