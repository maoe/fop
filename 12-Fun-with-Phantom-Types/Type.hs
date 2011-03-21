{-# LANGUAGE BangPatterns, GADTs #-}
module Type where
import Data.List (unfoldr)
import Data.Char (ord)
import Text.PrettyPrint.HughesPJ (Doc, (<>), ($$), int, char, text, nest)

data Type t where
  RInt  :: Type Int
  RChar :: Type Char
  RList :: Type a -> Type [a]
  RPair :: Type a -> Type b -> Type (a, b)

rString :: Type String
rString = RList RChar

type Bit = Bool

compress :: Type t -> t -> [Bit]
compress RInt          i      = compressInt i
compress RChar         c      = compressChar c
compress (RList ra)    []     = [False]
compress (RList ra)    (a:as) = True:compress ra a ++ compress (RList ra) as
compress (RPair ra rb) (a, b) = compress ra a ++ compress rb b

compressInt :: Int -> [Bit]
compressInt i = unfoldr (uncurry decimalToBinary) (32, i)

compressChar :: Char -> [Bit]
compressChar c = unfoldr (uncurry decimalToBinary) (7, ord c)

decimalToBinary :: Int -> Int -> Maybe (Bit, (Int, Int))
decimalToBinary  0  _ = Nothing
decimalToBinary !i !n = Just (m > 0, (pred i, d))
  where (!m, !d) = n `divMod` 2

pretty :: Type t -> t -> Doc
pretty RInt i               = int i
pretty RChar c              = char c
pretty (RList RChar) s      = text s
pretty (RList ra) []        = text "[]"
pretty (RList ra) (a:as)    = text "[" <> pretty ra a <> prettyL as
  where prettyL []          = text "]"
        prettyL (a:as)      = text "," <> pretty ra a <> prettyL as
pretty (RPair ra rb) (a, b) = text "("  <> pretty ra a
                           <> text ", " <> pretty rb b
                           <> text ")"

-- Exercise 12.3
eq :: Type t -> t -> t -> Bool
eq RInt  i1 i2 = i1 == i2
eq RChar c1 c2 = c1 == c2
