{-# LANGUAGE FlexibleContexts, PatternGuards #-}
module Groebner where

import Monomial
import Polynomial

nf :: (Fractional r, Ord v, Show v, Ord (Monomial o v))
   => Polynomial r o v -> [Polynomial r o v] -> Polynomial r o v
nf f s = go f
    where
      go h | h == 0      = 0
           | []    <- s' = h
           | (g:_) <- s' = go (spoly h g)
           where
             s' = [ g | g <- s, lm h `isDivisibleBy` lm g ]

groebner :: (Fractional r, Ord v, Show v, Ord (Monomial o v))
         => [Polynomial r o v] -> [Polynomial r o v]
groebner i = go i ps
    where
      ps = [ (f, g) | f <- i, g <- i, f /= g ]
      go s [] = s
      go s ps@((f, g):ps')
          | h == 0    = go s ps'
          | otherwise = go (h:s) (ps' ++ [ (h, f) | f <- s ])
          where
            h = nf (spoly f g) s
