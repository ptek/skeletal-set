{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Setoid.Equivalence (
  EquivalenceBy(..)
 ) where

class (Eq k, Ord k) => EquivalenceBy k a where
  eqRel :: a -> k
