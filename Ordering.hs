{-# LANGUAGE FlexibleInstances, EmptyDataDecls #-}
module Ordering where

import Degree

import Monomial (Monomial)
import qualified Monomial as Monomial
import Prelude hiding (lex)

class Ord a => Enumerable a where
    enumerate :: [a]

data Lex
data RevLex
data DegLex
data DegRevLex

lex' :: Ord v => Monomial o v -> Monomial o v -> [v] -> Bool
lex' a b []     = False
lex' a b (x:xs)
    = Monomial.exponent x a < Monomial.exponent x b
      || (Monomial.exponent x a == Monomial.exponent x b && lex' a b xs)

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
