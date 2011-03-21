{-# LANGUAGE GADTs #-}
module Term where

data Term t where
  Zero   :: Term Int
  Succ   :: Term Int -> Term Int
  Pred   :: Term Int -> Term Int
  IsZero :: Term Int -> Term Bool
  If     :: Term Bool -> Term a -> Term a -> Term a

eval :: Term t -> t
eval Zero       = 0
eval (Succ e)   = succ (eval e)
eval (Pred e)   = pred (eval e)
eval (IsZero e) = eval e == 0
eval (If p t f) = if eval p then eval t else eval f

one   = Succ Zero
true  = IsZero Zero
false = IsZero one
