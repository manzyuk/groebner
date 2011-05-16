{-# LANGUAGE FlexibleContexts #-}
module Polynomial
    ( Term
    , (*^)
    , Polynomial
    , constant
    , variable
    , lm
    , spoly
    )
    where

import Degree
import Monomial

import Data.Monoid

data Term r o v = T r (Monomial o v) deriving (Eq, Show)

instance (Num r, Ord v) => Monoid (Term r o v) where
    mempty = T 1 mempty
    T a m `mappend` T b n = T (a * b) (m `mappend` n)

-- Polynomials are represented as lists of non-zero terms, ordered in
-- descending order by their monomials.  This makes equality test and
-- extraction of the leading monomial very simple and fast.  Besides,
-- the relative order of terms does not change if the polynomial is
-- multiplied with a term.
newtype Polynomial r o v = P [Term r o v] deriving Eq

-- Multiply a polynomial with a term.
(*^) :: (Num r, Ord v) => Term r o v -> Polynomial r o v -> Polynomial r o v
u *^ P vs = P [ u `mappend` v | v <- vs ]

instance Ord v => HasDegree (Polynomial r o v) where
    degree (P []) = -1
    degree (P ts) = maximum [ degree m | T _ m <- ts ]

-- We are trying to make the display of polynomials as close to the
-- mathematical notation as possible.  Since we don't know what the
-- ground field 'r' can be, we apply some heuristics.
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

-- Arithmetic operations on polynomials are defined to preserve the
-- invariant of the representation of polynomials.
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
    -- Inclusion of 'abs' and 'signum' into 'Num' was a stupid idea.
    abs _ = error "abs is undefined for polynomials"
    signum _ = error "signum is undefined for polynomials"
    fromInteger = constant . fromInteger

-- View a constant 'c' as a polynomial (of degree 0 unless c is 0).
constant :: (Num r, Ord v) => r -> Polynomial r o v
constant 0 = P []
constant c = P [T c mempty]

-- View a variable 'v' as a polynomial (of degree 1).
variable :: (Num r, Eq v) => v -> Polynomial r o v
variable x = P [T 1 (inject x)]

-- Leading monomial of a polynomial.
lm :: Polynomial r o v -> Monomial o v
lm (P ((T _ m):_)) = m
lm (P [])          = error "lm: zero polynomial"

-- s-polynomial of a pair of polynomials.
spoly :: (Fractional r, Ord v, Show v, Ord (Monomial o v))
      => Polynomial r o v -> Polynomial r o v -> Polynomial r o v
spoly f@(P (u@(T a m):us)) g@(P (v@(T b n):vs)) = n' *^ f - m' *^ g
    where
      n' = T 1       (complement m n)
      m' = T (a / b) (complement n m)
