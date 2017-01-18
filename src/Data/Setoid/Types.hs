{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Data.Setoid.Types
Description : A strict implementation of Setoid
Copyright   : (c) Global Access Internet Services GmbH 2017
License     : BSDB-3
Maintainer  : pavlo@kerestey.net
-}
module Data.Setoid.Types where

import Data.Map.Strict (Map)
import GHC.Generics    (Generic)

-- | The Setoid type implemented as a wrapper around Map
newtype Setoid k a =
  Setoid (Map k a)
  deriving (Eq, Ord, Generic)
