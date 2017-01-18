{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Data.Setoid.Types
-- Copyright   : (c) Global Access Internet Services GmbH 2017
-- License     : BSD3
-- Maintainer  : Pavlo Kerestey <pavlo@kerestey.net>
--------------------------------------------------------------------------------
module Data.Setoid.Types where

import           Data.Map.Strict (Map)
import           GHC.Generics    (Generic)

newtype Setoid e a =
  Setoid (Map e a)
  deriving (Eq, Ord, Generic)
