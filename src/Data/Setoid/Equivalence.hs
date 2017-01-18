{-# LANGUAGE MultiParamTypeClasses #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Data.Setoid.Equivalence
-- Copyright   : (c) Global Access Internet Services GmbH 2017
-- License     : BSD3
-- Maintainer  : Pavlo Kerestey <pavlo@kerestey.net>
--------------------------------------------------------------------------------
module Data.Setoid.Equivalence
  ( EquivalenceBy(..)
  ) where

-- | Equivalence class. It reduces the data to the part which is
-- then being tested for equality in a Setoid.
class EquivalenceBy e a where
  eqRel :: a -> e
