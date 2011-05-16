{-# LANGUAGE FlexibleContexts, PatternGuards #-}
module Groebner (nf, groebner) where

import Monomial
import Polynomial

import Data.List

-- Normal form of a polynomial with respect to a list of polynomials
-- using Buchberger's algorithm.
nf :: (Fractional r, Ord v, Show v, Ord (Monomial v o))
   => Polynomial r v o -> [Polynomial r v o] -> Polynomial r v o
nf f s = go f
    where
      go h | h == 0      = 0
           | []    <- s' = h
           | (g:_) <- s' = go (spoly h g)
           where
             s' = [ g | g <- s, lm h `isDivisibleBy` lm g ]

-- Groebner basis algorithm.  Takes a list of generators of an ideal
-- and returns a Groebner basis of that ideal.
groebner :: (Fractional r, Ord v, Show v, Ord (Monomial v o))
         => [Polynomial r v o] -> [Polynomial r v o]
groebner i = go i ps
    where
      ps = [ (f, g) | f <- i, g <- i, f /= g, not (pc f g) ]
      go s [] = s
      go s ps@((f, g):ps')
          | h == 0    = go s ps'
          | otherwise = go (h:s) (ps' ++ [ (h, f) | f <- s, not (pc h f) ])
          where
            h = nf (spoly f g) s

-- Product criterion: if the lcm of the leading monomials of f and g
-- is their product, then the s-poly of f and g reduces to 0 w.r.t.
-- the set {f, g}.
pc f g = null (variables (lm f) `intersect` variables (lm g))
