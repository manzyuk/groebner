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

class Ord a => Enumerable a where
    enumerate :: [a]

data Lex
data RevLex
data DegLex
data DegRevLex

lex' :: Ord v => Monomial o v -> Monomial o v -> [v] -> Bool
lex' a b []     = False
lex' a b (x:xs) = exponent x a < exponent x b
                  || (exponent x a == exponent x b && lex' a b xs)

lex, revlex, deglex, degrevlex :: Enumerable v
                               => Monomial o v -> Monomial o v -> Bool
lex       a b = lex' a b enumerate
revlex    a b = lex' a b (reverse enumerate)
deglex    a b = degree a < degree b
                || (degree a == degree b && a `lex` b)
degrevlex a b = degree a < degree b
                || (degree a == degree b && b `revlex` a)

instance Enumerable v => Ord (Monomial Lex v) where
    (<) = lex

instance Enumerable v => Ord (Monomial RevLex v) where
    (<) = revlex

instance Enumerable v => Ord (Monomial DegLex v) where
    (<) = deglex

instance Enumerable v => Ord (Monomial DegRevLex v) where
    (<) = degrevlex
