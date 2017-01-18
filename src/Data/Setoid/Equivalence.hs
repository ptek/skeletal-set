{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Data.Setoid.Equivalence
Description : A strict implementation of Setoid
Copyright   : (c) Global Access Internet Services GmbH 2017
License     : BSDB-3
Maintainer  : pavlo@kerestey.net
Stability   : alpha
Portability : POSIX
-}
module Data.Setoid.Equivalence
  ( EquivalenceBy(..)
  ) where

-- | Equivalence by class. It reduces the data to the part which is
-- then being tested for equality in a Setoid.
class EquivalenceBy e a where
  eqRel :: a -> e
