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

-- Reader
newtype Reader r i o = R ((r, i) -> o)

instance Category (Reader r) where
  id        = R snd
  R f . R g = R $ uncurry $ \r -> curry f r . curry g r

instance Arrow (Reader r) where
  arr         = R . uncurry . const
  first (R f) = R $ first f . unassoc

-- Writer
newtype Monoid m => Writer m i o = W (i -> (m, o))

instance Monoid m => Category (Writer m) where
  id        = W (mempty,)
  W f . W g = W $ \i -> let (m,  o)  = g i
                            (m', o') = f o
                        in (m `mappend` m', o')

instance Monoid m => Arrow (Writer m) where
  arr f       = W $ (mempty,) . f
  first (W f) = W $ assoc . first f


-- exercise 10.2
newtype ListMap i o = LM ([i] -> [o])

instance Category ListMap where
  id          = LM id
  LM f . LM g = LM (f . g)

instance Arrow ListMap where
  arr          = LM . fmap
  first (LM f) = LM $ \xs -> let (is, ts) = unzip xs
                             in uncurry zip (f is, ts)

{-
  TODO
-}

-- exercise 10.3

data Stream a = Cons a (Stream a)

instance Functor Stream where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

newtype StreamMap i o = SM (Stream i -> Stream o)

instance Category StreamMap where
  id          = SM id
  SM f . SM g = SM (f . g)

instance Arrow StreamMap where
  arr          = SM . fmap
  first (SM f) = SM $ stream . first f . unstream

stream :: (Stream a, b) -> Stream (a, b)
stream (s, t) = fmap (,t) s

unstream :: Stream (a, b) -> (Stream a, b)
unstream (Cons (x, y) xs) = (Cons x (fst $ unstream xs), y)

-- exercise 10.4
(|><) :: Arrow (~>) => a ~> b -> (a -> b) -> (a, a) ~> (b, b)
f |>< g = first f >>> arr (id *** g)

{-
  TODO
-}


--
-- Special cases
--

-- ArrowApply
curryA :: Arrow (~>) => (a, b) ~> c -> a -> b ~> c
curryA f b = mkPair b >>> f

mkPair :: Arrow (~>) => a -> b ~> (a, b)
mkPair = arr . (,)

instance ArrowApply (State s) where

instance ArrowApply NonDet where

instance ArrowApply Auto where
  app = arr $ \(A f, x) -> fst (f x)

-- ArrowChoice
assocsum :: Either (Either a b) c -> Either a (Either b c)
assocsum (Left (Left a))  = Left a
assocsum (Left (Right a)) = Right (Left a)
assocsum (Right a)        = Right (Right a)

distr :: (Either a b, c) -> Either (a, c) (b, c)
distr (Left a, c)  = Left (a, c)
distr (Right b, c) = Right (b, c)

instance ArrowChoice (State s) where

instance ArrowChoice NonDet where

instance ArrowChoice Auto where
  left (A f) = A lf
    where lf (Left i)  = let (o, f') = f i
                         in (Left o, left f')
          lf (Right i) = (Right i, left (A f))

instance ArrowChoice StreamMap where


newtype Except a b c = E (a b (Either String c))

instance Category (Except (~>)) where

instance ArrowChoice (~>) => Arrow (Except (~>))


--
trace :: ((a, c) -> (b, c)) -> a -> b
trace f a = let (b, c) = f (a, c) in b

instance ArrowLoop (State s) where
  loop (ST f) = ST $ trace $ unassoc . f . assoc

instance ArrowLoop (MapTrans s) where
  loop (MT f) = MT $ trace $ unzipMap . f . zipMap

instance ArrowLoop Auto where
  loop (A f) = A $ \i -> let (~(o, o'), f') = f (i, o')
                         in (o, loop f')

instance ArrowLoop StreamMap where