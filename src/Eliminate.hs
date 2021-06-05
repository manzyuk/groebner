{-# LANGUAGE FlexibleContexts, TypeOperators #-}
module Eliminate (eliminate) where

import Groebner
import Monomial
import Ordering
import Polynomial
import Types

import Data.Monoid

-- Elimination of variables.  We consider only a particular case where
-- the elimination ordering used is a block ordering.  Takes a list of
-- generators of an ideal over r[v1, v2] and returns a Groebner basis
-- of the intersection of that ideal with the ring r[v2].
eliminate :: ( Eq r
             , Fractional r
             , Ord v1, Ord v2
             , Show v1, Show v2
             , Ord (Monomial v1 o1)
             , Ord (Monomial v2 o2))
          => [Polynomial r (v1 :<: v2) (o1, o2)] -> [Polynomial r v1 o1]
eliminate i = [ demote f | f <- groebner i
                         , let (xa, yb) = uninterleave (lm f)
                         , yb == mempty ]
