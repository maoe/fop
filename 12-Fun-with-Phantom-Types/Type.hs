{-# LANGUAGE GADTs, TemplateHaskell, StandaloneDeriving, RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}
module Type where
import Control.Applicative       (Alternative(..), (<$>), (<*>), pure)
import Control.Arrow             -- ((***))
import Control.Monad.State       (State, get, put, evalState)
import Data.Char                 (ord, chr, isAscii)
import Data.Int                  (Int32)
import Data.List                 (unfoldr)
import Data.Maybe                (isJust, fromJust)
import Test.QuickCheck
import Test.QuickCheck.All       (quickCheckAll)
import Text.PrettyPrint.HughesPJ (Doc, (<>), char, text, int)
import Prelude hiding (compare, zip, take, head)
import qualified Prelude as P (compare, head)

-- | Type representation
data Type t where
  RInt    :: Type Int32
  RChar   :: Type Char
  RList   :: Type a -> Type [a]
  RPair   :: Type a -> Type b -> Type (a, b)
  RDyn    :: Type Dynamic
  RFun    :: Type a -> Type b -> Type (a -> b)
  RPerson :: Type Person

rString :: Type String
rString = RList RChar

deriving instance Show (Type t)

data Dynamic where
  Dyn :: Type t -> t -> Dynamic

type Name = String

type Age  = Int32

data Person = Person Name Age
            deriving (Ord, Eq)

tick :: Name -> Traversal
tick s RPerson (Person n a)
  | s == n       = Person n (a + 1)
tick _ _       p = p

type Traversal = forall t. Type t -> t -> t

copy :: Traversal
copy _ = id

(<.>) :: Traversal -> Traversal -> Traversal
(f <.> g) rt = f rt . g rt

imap :: Traversal -> Traversal
imap _ RInt          i            = i
imap _ RChar         c            = c
imap _ (RList _)     []           = []
imap f (RList ra)    (a:as)       = f ra a:f (RList ra) as
imap f (RPair ra rb) (a, b)       = (f ra a, f rb b)
imap f (RFun ra rb)  g            = undefined
imap f RDyn          (Dyn ra a)   = Dyn ra (f ra a)
imap f RPerson       (Person n a) = Person (f rString n) (f RInt a)

everywhere :: Traversal -> Traversal
everywhere f = f <.> imap (everywhere f)

everywhere' :: Traversal -> Traversal
everywhere' f = imap (everywhere' f) <.> f

type Query q = forall t. Type t -> t -> q

isum :: Query Int -> Query Int
isum _ RInt          _            = 0
isum _ RChar         _            = 0
isum _ (RList _)     []           = 0
isum f (RList ra)    (a:as)       = f ra a + f (RList ra) as
isum f (RPair ra rb) (a, b)       = f ra a + f rb b
isum f (RFun ra rb)  g            = undefined
isum f RDyn          (Dyn ra a)   = f ra a
isum f RPerson       (Person n a) = f rString n + f RInt a

total :: Query Int -> Query Int
total f rt t = f rt t + isum (total f) rt t

-- | Bit representation
data Bit = O | I deriving (Show, Enum)

bit :: Bool -> Bit
bit False = O
bit True  = I

-- | Generic compresssion
compress :: Type t -> t -> [Bit]
compress RInt          i         = compressInt i
compress RChar         c         = compressChar c
compress (RList _)     []        = [O]
compress (RList ra)    (a:as)    = I:compress ra a ++ compress (RList ra) as
compress (RPair ra rb) (a, b)    = compress ra a ++ compress rb b
compress (RFun ra rb)  f         = error "a function is not allowed to be serialized"
compress RPerson       p         = compressPerson p
compress RDyn          (Dyn t a) = compressRep (Rep t) ++ compress t a

-- | Generic uncompresssion
uncompress :: Type t -> [Bit] -> t
uncompress ra = evalState (uncompress' ra)

uncompress' :: Type t -> Uncompressor t
uncompress' RInt          = uncompressInt
uncompress' RChar         = uncompressChar
uncompress' (RList ra)    = uncompressList ra
uncompress' (RPair ra rb) = uncompressPair ra rb
uncompress' (RFun _ _)    = error "a function is not allowed to be serialized"
uncompress' RPerson       = undefined
uncompress' RDyn          = uncompressDyn

type Uncompressor a = State [Bit] a

take :: Int -> Uncompressor [Bit]
take n = do
  (xs, ys) <- splitAt n <$> get
  put ys
  return xs

head :: Uncompressor Bit
head = P.head <$> take 1

uncompressInt :: Uncompressor Int32
uncompressInt = foldr bitToDecimal 0 <$> take 32

uncompressChar :: Uncompressor Char
uncompressChar = chr . fromIntegral . foldr bitToDecimal 0 <$> take 7

uncompressList :: Type a -> Uncompressor [a]
uncompressList ra = do
  b <- head
  case b of
    O -> pure []
    I -> (:) <$> uncompress' ra <*> uncompressList ra

uncompressPair :: Type a -> Type b ->  Uncompressor (a, b)
uncompressPair ra rb = (,) <$> uncompress' ra <*> uncompress' rb

uncompressDyn :: Uncompressor Dynamic
uncompressDyn = do
  Rep t <- uncompressRep'
  Dyn t <$> uncompress' t

compressInt :: Int32 -> [Bit]
compressInt i = unfoldr (uncurry decimalToBit) (32, i)

compressChar :: Char -> [Bit]
compressChar c = unfoldr (uncurry decimalToBit) (7, fromIntegral (ord c))

compressPerson :: Person -> [Bit]
compressPerson (Person n a) = compress rString n ++ compress RInt a

decimalToBit :: Int -> Int32 -> Maybe (Bit, (Int, Int32))
decimalToBit 0 _ = Nothing
decimalToBit i n = Just (bit (m > 0), (pred i, d))
  where (d, m) = n `divMod` 2

bitToDecimal :: Bit -> Int32 -> Int32
bitToDecimal b n = n*2 + fromIntegral (fromEnum b)

pretty :: Type t -> t -> Doc
pretty RInt          i            = int $ fromIntegral i
pretty RChar         c            = char c
pretty (RList RChar) s            = text s
pretty (RList _)     []           = text "[]"
pretty (RList ra)    (a:as)       = text "[" <> pretty ra a <> prettyL as
  where prettyL = foldr (\a' -> (text "," <> pretty ra a' <>)) (text "]")
pretty (RPair ra rb) (a, b)       = text "("  <> pretty ra a <>
                                    text ", " <> pretty rb b <>
                                    text ")"
pretty (RFun _ _)    _            = error "a function is not allowed to be serialized"
pretty RPerson       (Person n a) = text n <> text (show a)
pretty RDyn          (Dyn t ra)   = text "Dyn" <> pretty t ra

-- Exercise 12.3
eq :: Type t -> t -> t -> Bool
eq RInt          a           b           = a == b
eq RChar         a           b           = a == b
eq (RList _)     []          []          = True
eq (RList _)     (_:_)       []          = False
eq (RList _)     []          (_:_)       = False
eq t@(RList ra)  (a:as)      (b:bs)      = eq ra a b && eq t as bs
eq (RPair ra rb) (a, c)      (b, d)      = eq ra a b && eq rb c d
eq (RFun _ _)    _           _           = error "functions are not comparable"
eq RPerson       a           b           = a == b
eq RDyn          (Dyn ta ra) (Dyn tb rb) = Rep ta == Rep tb &&
                                           isJust mrb &&
                                           eq ta ra (fromJust mrb)
  where mrb = cast (Dyn tb rb) ta

compare :: Type t -> t -> t -> Ordering
compare RInt          a           b            = P.compare a b
compare RChar         a           b            = P.compare a b
compare (RList _)     []          []           = EQ
compare (RList _)     (_:_)       []           = GT
compare (RList _)     []          (_:_)        = LT
compare t@(RList ra)  (a:as)      (b:bs)       =
  case compare ra a b of
    EQ       -> compare t as bs
    ordering -> ordering
compare (RPair ra rb) (a, c)      (b, d)       =
  case compare ra a b of
    EQ       -> compare rb c d
    ordering -> ordering
compare (RFun _ _)     _           _           = error "functions are not comparable"
compare RPerson        a           b           = P.compare a b
compare RDyn           (Dyn ta ra) (Dyn tb rb) =
  case tequal ta tb of
    Just f  -> compare tb (f ra) rb
    Nothing -> error "type error"

-- Dynamic values
tequal :: Type t -> Type r -> Maybe (t -> r)
tequal RInt            RInt            = pure id
tequal RChar           RChar           = pure id
tequal (RList ra1)     (RList ra2)     = fmap  <$> tequal ra1 ra2
tequal (RPair ra1 rb1) (RPair ra2 rb2) = (***) <$> tequal ra1 ra2 <*> tequal rb1 rb2
tequal (RFun ra1 rb1)  (RFun ra2 rb2)  = (\f g h -> g . h . f)
                                           <$> tequal ra2 ra1
                                           <*> tequal rb1 rb2
tequal _               _               = empty

cast :: Dynamic -> Type t -> Maybe t
cast (Dyn ra a) rt = tequal ra rt <*> pure a

-- Exercise 12.6
data Rep where
  Rep :: Type t -> Rep

compressRep :: Rep -> [Bit]
compressRep (Rep RInt)          = [I,I,I]
compressRep (Rep RChar)         = [I,I,O]
compressRep (Rep (RList ra))    = [I,O,I] ++ compressRep (Rep ra)
compressRep (Rep (RPair ra rb)) = [I,O,O] ++ compressRep (Rep ra) ++ compressRep (Rep rb)
compressRep (Rep (RFun ra rb))  = [O,I,I] ++ compressRep (Rep ra) ++ compressRep (Rep rb)
compressRep (Rep RDyn)          = [O,I,O]
compressRep (Rep RPerson)       = [O,O,I]

uncompressRep :: [Bit] -> Rep
uncompressRep = evalState uncompressRep'

uncompressRep' :: Uncompressor Rep
uncompressRep' = do
  bs <- take 3
  case bs of
    [I,I,I] -> return $ Rep RInt
    [I,I,O] -> return $ Rep RChar
    [I,O,I] -> do Rep ra <- uncompressRep'
                  return $ Rep (RList ra)
    [I,O,O] -> do Rep ra <- uncompressRep'
                  Rep rb <- uncompressRep'
                  return $ Rep (RPair ra rb)
    [O,I,O] -> return $ Rep RDyn
    [O,O,I] -> return $ Rep RPerson
    _       -> fail "invalid input"

-- Propterties
prop_compress_Int :: NonNegative Int32 -> Bool
prop_compress_Int (NonNegative n) = n == uncompress RInt (compress RInt n)

prop_compress_Char :: Char -> Property
prop_compress_Char c = isAscii c ==> c == uncompress RChar (compress RChar c)

-- prop_IdempotentRep :: Rep -> Bool
-- prop_IdempotentRep r = r == uncompressRep (compressRep r)

instance Arbitrary Rep where
  arbitrary = oneof [ return $ Rep RInt
                    , return $ Rep RChar
                    , do Rep t <- arbitrary
                         return $ Rep (RList t)
                    , do Rep ta <- arbitrary
                         Rep tb <- arbitrary
                         return $ Rep (RPair ta tb)
                    ]

instance Eq Rep where
  Rep ra == Rep rb = isJust $ tequal ra rb

test :: IO Bool
test = $quickCheckAll
