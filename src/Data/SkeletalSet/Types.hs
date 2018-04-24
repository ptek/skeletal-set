{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Data.SkeletalSet.Types
-- Copyright   : (c) Global Access Internet Services GmbH 2017
-- License     : BSD3
-- Maintainer  : Pavlo Kerestey <pavlo@kerestey.net>
--------------------------------------------------------------------------------
module Data.SkeletalSet.Types where

import           Data.Map.Strict (Map)
import           GHC.Generics    (Generic)

newtype SkeletalSet e a =
  SkeletalSet (Map e a)
  deriving (Eq, Ord, Generic)
