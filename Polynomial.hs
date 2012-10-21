{-# LANGUAGE FlexibleContexts, TypeOperators #-}
module Polynomial
    ( Term
    , (*^)
    , Polynomial
    , constant
    , variable
    , lm
    , spoly
    , demote
    )
    where

import Monomial
import Types

import Data.Monoid

data Term r v o = T r (Monomial v o) deriving (Eq, Show)

instance (Num r, Ord v) => Monoid (Term r v o) where
    mempty = T 1 mempty
    T a m `mappend` T b n = T (a * b) (m `mappend` n)

-- Polynomials are represented as lists of non-zero terms, ordered in
-- descending order by their monomials.  This makes equality test and
-- extraction of the leading monomial very simple and fast.  Besides,
-- the relative order of terms does not change if the polynomial is
-- multiplied with a term.
newtype Polynomial r v o = P [Term r v o] deriving Eq

-- Multiply a polynomial with a term.
(*^) :: (Num r, Ord v) => Term r v o -> Polynomial r v o -> Polynomial r v o
u *^ P vs = P [ u `mappend` v | v <- vs ]

instance Ord v => HasDegree (Polynomial r v o) where
    degree (P []) = -1
    degree (P ts) = maximum [ degree m | T _ m <- ts ]

-- We are trying to make the display of polynomials as close to the
-- mathematical notation as possible.  Since we don't know what the
-- ground field 'r' can be, we apply some heuristics.
instance (Eq r, Show r, Num r, Ord v, Show v) => Show (Polynomial r v o) where
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
instance (Eq r, Num r, Ord v, Show v, Ord (Monomial v o))
    => Num (Polynomial r v o) where
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
constant :: (Eq r, Num r, Ord v) => r -> Polynomial r v o
constant 0 = P []
constant c = P [T c mempty]

-- View a variable 'v' as a polynomial (of degree 1).
variable :: (Num r, Eq v) => v -> Polynomial r v o
variable x = P [T 1 (inject x)]

-- Leading monomial of a polynomial.
lm :: Polynomial r v o -> Monomial v o
lm (P ((T _ m):_)) = m
lm (P [])          = error "lm: zero polynomial"

-- s-polynomial of a pair of polynomials.
spoly :: (Eq r, Fractional r, Ord v, Show v, Ord (Monomial v o))
      => Polynomial r v o -> Polynomial r v o -> Polynomial r v o
spoly f@(P (u@(T a m):us)) g@(P (v@(T b n):vs)) = n' *^ f - m' *^ g
    where
      n' = T 1       (complement m n)
      m' = T (a / b) (complement n m)

-- The following function is only used when the input polynomial is
-- effectively a polynomial from r[v1] to adjust its type.
demote :: ( Fractional r
          , Ord v1, Ord v2
          , Show v1, Show v2
          , Ord (Monomial v1 o1)
          , Ord (Monomial v2 o2))
       => Polynomial r (v1 :<: v2) (o1, o2) -> Polynomial r v1 o1
demote (P us) = P [ T c x | T c z <- us
                          , let (x, _) = uninterleave z ]
