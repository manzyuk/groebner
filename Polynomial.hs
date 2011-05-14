{-# LANGUAGE FlexibleContexts #-}
module Polynomial where

import Monomial

import Data.Monoid

data Term r o v = T r (Monomial o v) deriving (Eq, Show)

instance (Num r, Ord v) => Monoid (Term r o v) where
    mempty = T 1 mempty
    T a m `mappend` T b n = T (a * b) (m `mappend` n)

newtype Polynomial r o v = P [Term r o v] deriving Eq

(*^) :: (Num r, Ord v) => Term r o v -> Polynomial r o v -> Polynomial r o v
u *^ P vs = P [ u `mappend` v | v <- vs ]

instance Ord v => HasTotalDegree (Polynomial r o v) where
    totalDegree (P []) = -1
    totalDegree (P ts) = maximum [ totalDegree m | T _ m <- ts ]

instance (Num r, Ord v, Show v) => Show (Polynomial r o v) where
    show (P [])     = "0"
    show (P (t:ts)) = showHead t ++ showTail ts
        where
          showHead (T c m) = prefix ++ show m
              where
                prefix = case c of
                           1  -> ""
                           -1 -> "-"
                           _  -> show c
          showTerm (T c m) = prefix ++ show m
              where
                prefix = case signum c of
                           1  -> '+':a
                           -1 -> '-':a
                           _  -> "(" ++ show c ++ ")"
                a = if abs c == 1 then "" else show (abs c)
          showTail = concatMap showTerm

instance (Num r, Ord v, Show v, Ord (Monomial o v))
    => Num (Polynomial r o v) where
    f@(P (u@(T a m):us)) + g@(P (v@(T b n):vs))
        | m == n && a + b /= 0
        = let P ws = P us + P vs in P $ T (a + b) m:ws
        | m == n && a + b == 0
        = P us + P vs
        | m < n
        = let P ws = f + P vs in P $ v:ws
        | otherwise
        = let P ws = P us + g in P $ u:ws
    f + P [] = f
    P [] + g = g

    P (u:us) * P (v:vs)
        = let P ws = P us * P vs + u *^ P vs + v *^ P us
          in P $ (u `mappend` v):ws
    _ * P [] = P []
    P [] * _ = P []

    negate (P ts) = P $ [ T (negate a) m | T a m <- ts ]
    abs _ = error "abs is undefined for polynomials"
    signum _ = error "signum is undefined for polynomials"
    fromInteger = constant . fromInteger

constant :: (Num r, Ord v) => r -> Polynomial r o v
constant 0 = P []
constant c = P [T c mempty]

variable :: (Num r, Eq v) => v -> Polynomial r o v
variable x = P [T 1 (inject x)]

lm :: Polynomial r o v -> Monomial o v
lm (P ((T _ m):_)) = m
lm (P [])          = error "lm: zero polynomial"

spoly :: (Fractional r, Ord v, Show v, Ord (Monomial o v))
      => Polynomial r o v -> Polynomial r o v -> Polynomial r o v
spoly f@(P (u@(T a m):us)) g@(P (v@(T b n):vs)) = n' *^ f - m' *^ g
    where
      n' = T 1       (complement m n)
      m' = T (a / b) (complement n m)
