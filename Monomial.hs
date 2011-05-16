module Monomial
    ( Monomial
    , inject
    , toList
    , fromList
    , exponent
    , complement
    , isDivisibleBy
    )
    where

import Degree

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe
import Data.Monoid

import Prelude hiding (exponent, lcm, div)

-- A monomial is represented internally by a map from variables to
-- integers (exponents).  The variable 'o' is used a tag allowing
-- us to define different instances of some class on 'Monomial'.
newtype Monomial o v = M (Map v Int) deriving Eq

-- View a variable as a monomial.
inject :: Eq v => v -> Monomial o v
inject x = M $ Map.singleton x 1

-- Convert a monomial to a list of variable-exponent pairs.  The
-- variables with zero exponents are dropped.
toList :: Ord v => Monomial o v -> [(v, Int)]
toList (M m) = [ p | p@(x, n) <- Map.toList m, n /= 0 ]

-- Build a monomial from a list of variable-exponent pairs.
fromList :: Ord v => [(v, Int)] -> Monomial o v
fromList xs = M $ Map.fromList [ p | p@(x, n) <- xs, n /= 0 ]

-- Exponent of a variable in a monomial.
exponent :: Ord v => v -> Monomial o v -> Int
exponent x (M m) = fromMaybe 0 (Map.lookup x m)

instance (Ord v, Show v) => Show (Monomial o v) where
    show m
        | null support
        = "1"
        | otherwise
        = concat [ show x ++ suffix
                 | (x, n) <- support
                 , let suffix = if n == 1
                                then ""
                                else "^" ++ show n
                 ]
        where
          support = toList m

instance Ord v => Monoid (Monomial o v) where
    mempty = M Map.empty
    M a `mappend` M b = M $ Map.unionWith (+) a b

instance Ord v => HasDegree (Monomial o v) where
    degree (M m) = Map.fold (+) 0 m

-- Least common multiple of monomials.
lcm :: Ord v => Monomial o v -> Monomial o v -> Monomial o v
lcm (M a) (M b) = M $ Map.unionWith max a b

-- Divide one monomial by another.  This is used only when one
-- monomial is divisible by the other, but we give a plausible
-- definition in the general case.
div :: Ord v => Monomial o v -> Monomial o v -> Monomial o v
div (M a) (M b) = M $ Map.differenceWith sub a b
    where
      sub x y | x > y     = Just (x - y)
              | otherwise = Nothing

-- 'complement m n' is the product of factors in 'n' that are
-- missing in 'm'.
complement :: Ord v => Monomial o v -> Monomial o v -> Monomial o v
complement m n = lcm m n `div` m

-- Test whether one monomial is divisible by another.
isDivisibleBy :: Ord v => Monomial o v -> Monomial o v -> Bool
isDivisibleBy (M a) (M b) = Map.isSubmapOfBy (<=) b a
