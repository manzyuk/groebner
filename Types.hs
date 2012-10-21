{-# LANGUAGE TypeOperators, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts  #-}
{-# LANGUAGE OverlappingInstances                 #-}
module Types where

-- Both monomials and polynomials have degree, so it is convenient to
-- have an overloaded function 'degree'.
class HasDegree a where
    degree :: a -> Int

-- Class of enumerable types.  We add 'Ord' to the context in order to
-- save some typing (and because it makes sense).
class Ord a => Enumerable a where
    enumerate :: [a]

data a :<: b = Inl a | Inr b deriving (Eq, Ord)
infixr 6 :<:

instance (Show a, Show b) => Show (a :<: b) where
    show (Inl x) = show x
    show (Inr x) = show x

instance (Enumerable a, Enumerable b) => Enumerable (a :<: b) where
    enumerate = map Inl enumerate ++ map Inr enumerate

class Sub a b where
    inj :: a -> b

instance Sub a a where
    inj = id

instance Sub a (a :<: b) where
    inj = Inl

instance Sub a c => Sub a (b :<: c) where
    inj = Inr . inj
