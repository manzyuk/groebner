{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls, TypeOperators       #-}
module Ordering
    ( Lex
    , RevLex
    , DegLex
    , DegRevLex
    , Enumerable (..)
    )
    where

import Monomial (Monomial, exponent, uninterleave)
import Types

import Prelude hiding (lex, exponent)

data Lex         -- Lexicographic ordering
data RevLex      -- Reverse lexicographic ordering
data DegLex      -- Degree lexicographic ordering
data DegRevLex   -- Reverse degree lexicographic ordering

-- The following definitions are written in a slightly unintuitive
-- style because they define (<=), not (>) or (<) as is customary.
-- This is necessary because a minimal instance declaration of Ord
-- requires either 'compare' or (<=).  In particular, if we define
-- only (>), then the default definition of (<=) isn't in terms of
-- (>) but in terms of 'compare', which in turn by default is
-- defined in terms of (<=), leading to an infinite loop.
lex' :: (Ord v, Show v) => Monomial v o -> Monomial v o -> [v] -> Bool
lex' a b []     = True
lex' a b (x:xs) = exponent x a <= exponent x b
                  && (exponent x a /= exponent x b || lex' a b xs)

lex, revlex, deglex, degrevlex :: (Enumerable v, Show v)
                               => Monomial v o -> Monomial v o -> Bool
lex       a b = lex' a b enumerate
revlex    a b = lex' a b (reverse enumerate)
deglex    a b = degree a <= degree b
                && (degree a /= degree b || a `lex` b)
degrevlex a b = degree a <= degree b
                && (degree a /= degree b || b `revlex` a)

-- The type variable 'o' in 'Monomial o v' is used as a tag, so that
-- we can define different 'Ord' instances on the 'Monomial' type.
instance (Show v, Enumerable v) => Ord (Monomial v Lex) where
    (<=) = lex

instance (Show v, Enumerable v) => Ord (Monomial v RevLex) where
    (<=) = revlex

instance (Show v, Enumerable v) => Ord (Monomial v DegLex) where
    (<=) = deglex

instance (Show v, Enumerable v) => Ord (Monomial v DegRevLex) where
    (<=) = degrevlex

-- Product (or block) orderings
instance ( Ord v1, Ord v2
         , Show v1, Show v2
         , Ord (Monomial v1 o1)
         , Ord (Monomial v2 o2))
    => Ord (Monomial (v1 :>: v2) (o1, o2)) where
        z1 <= z2 = x1 <= x2 && (x1 /= x2 || y1 <= y2)
            where
              (x1, y1) = uninterleave z1
              (x2, y2) = uninterleave z2