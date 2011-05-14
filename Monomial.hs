module Monomial
    ( Monomial
    , inject
    , degree
    , support
    , complement
    , isDivisibleBy
    , HasTotalDegree (..)
    )
    where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe
import Data.Monoid

import Prelude hiding (lcm, div)

newtype Monomial o v = M (Map v Int) deriving Eq

inject :: Eq v => v -> Monomial o v
inject x = M $ Map.singleton x 1

degree :: Ord v => v -> Monomial o v -> Int
degree x (M m) = fromMaybe 0 (Map.lookup x m)

support :: Ord v => Monomial o v -> [(v, Int)]
support (M m) = [ p | p@(x, n) <- Map.toList m, n /= 0 ]

instance (Ord v, Show v) => Show (Monomial o v) where
    show m
        | null sup
        = "1"
        | otherwise
        = concat [ show x ++ suffix
                 | (x, n) <- sup
                 , let suffix = if n == 1
                                then ""
                                else "^" ++ show n
                 ]
        where
          sup = support m

instance Ord v => Monoid (Monomial o v) where
    mempty = M Map.empty
    M a `mappend` M b = M $ Map.unionWith (+) a b

class HasTotalDegree a where
    totalDegree :: a -> Int

instance Ord v => HasTotalDegree (Monomial o v) where
    totalDegree (M m) = Map.fold (+) 0 m

lcm :: Ord v => Monomial o v -> Monomial o v -> Monomial o v
lcm (M a) (M b) = M $ Map.unionWith max a b

div :: Ord v => Monomial o v -> Monomial o v -> Monomial o v
div (M a) (M b) = M $ Map.differenceWith sub a b
    where
      sub x y | x > y     = Just (x - y)
              | otherwise = Nothing

complement :: Ord v => Monomial o v -> Monomial o v -> Monomial o v
complement m n = lcm m n `div` m

isDivisibleBy :: Ord v => Monomial o v -> Monomial o v -> Bool
isDivisibleBy (M a) (M b) = Map.isSubmapOfBy (<=) b a
