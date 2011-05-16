module Degree (HasDegree (..)) where

-- Both monomials and polynomials have degree, so it is convenient to
-- have an overloaded function 'degree'.
class HasDegree a where
    degree :: a -> Int