{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Types where

import Data.Proxy

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

data Nat = Zero | Succ Nat

type family Index (t :: *) (u :: *) :: Nat where
  Index t t = Zero
  Index t (t :<: u) = Zero
  Index t (u :<: v) = Succ (Index t v)

type family IsLast (t :: *) (u :: *) :: Bool where
  IsLast t t = True
  IsLast t (t :<: u) = False
  IsLast t (u :<: v) = IsLast t v

class SubN (n :: Nat) (isLast :: Bool) a b where
  injN :: Proxy n -> Proxy isLast -> a -> b

instance SubN Zero True a a where
  injN _ _ = id

instance SubN Zero False a (a :<: b) where
  injN _ _ = Inl

instance SubN n isLast a c => SubN (Succ n) isLast a (b :<: c) where
  injN _ _ = Inr . injN (Proxy :: Proxy n) (Proxy :: Proxy isLast)

type Sub a b = SubN (Index a b) (IsLast a b) a b

inj :: forall a b. Sub a b => a -> b
inj = injN (Proxy :: Proxy (Index a b)) (Proxy :: Proxy (IsLast a b))
