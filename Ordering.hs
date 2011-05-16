{-# LANGUAGE FlexibleInstances, EmptyDataDecls #-}
module Ordering
    ( Lex
    , RevLex
    , DegLex
    , DegRevLex
    , Enumerable (..)
    )
    where

import Degree
import Monomial (Monomial, exponent)

import Prelude hiding (lex, exponent)

-- Class of enumerable types.  We add 'Ord' to the context in order to
-- save some typing (and because it makes sense).
class Ord a => Enumerable a where
    enumerate :: [a]

data Lex         -- Lexicographic ordering
data RevLex      -- Reverse lexicographic ordering
data DegLex      -- Degree lexicographic ordering
data DegRevLex   -- Reverse degree lexicographic ordering

lex' :: Ord v => Monomial v o -> Monomial v o -> [v] -> Bool
lex' a b []     = False
lex' a b (x:xs) = exponent x a < exponent x b
                  || (exponent x a == exponent x b && lex' a b xs)

lex, revlex, deglex, degrevlex :: Enumerable v
                               => Monomial v o -> Monomial v o -> Bool
lex       a b = lex' a b enumerate
revlex    a b = lex' a b (reverse enumerate)
deglex    a b = degree a < degree b
                || (degree a == degree b && a `lex` b)
degrevlex a b = degree a < degree b
                || (degree a == degree b && b `revlex` a)

-- The type variable 'o' in 'Monomial o v' is used as a tag, so that
-- we can define different 'Ord' instances on the 'Monomial' type.
instance Enumerable v => Ord (Monomial v Lex) where
    (<) = lex

instance Enumerable v => Ord (Monomial v RevLex) where
    (<) = revlex

instance Enumerable v => Ord (Monomial v DegLex) where
    (<) = deglex

instance Enumerable v => Ord (Monomial v DegRevLex) where
    (<) = degrevlex
