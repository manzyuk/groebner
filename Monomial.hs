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

newtype Monomial o v = M (Map v Int) deriving Eq

inject :: Eq v => v -> Monomial o v
inject x = M $ Map.singleton x 1

toList :: Ord v => Monomial o v -> [(v, Int)]
toList (M m) = [ p | p@(x, n) <- Map.toList m, n /= 0 ]

fromList :: Ord v => [(v, Int)] -> Monomial o v
fromList xs = M $ Map.fromList [ p | p@(x, n) <- xs, n /= 0 ]

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
