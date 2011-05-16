module Degree (HasDegree (..)) where

class HasDegree a where
    degree :: a -> Int