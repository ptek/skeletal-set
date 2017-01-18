{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : Data.Setoid
Description : A strict implementation of Setoid
Copyright   : (c) Global Access Internet Services GmbH 2017
License     : BSDB-3
Maintainer  : pavlo@kerestey.net
-}
module Data.Setoid
  ( Setoid
    -- * Class
  , EquivalenceBy(..)
    -- * Operators
  , (=~=)
  , (\\)
  , (∪)
    -- * Construction
  , empty
  , ø
  , singleton
  , union
  , unions
  , unionWith
    -- * Difference
  , difference
    -- * Query
  , member
  , null
  , equivalence
    -- * Conversion
  , fromList
  , fromListWith
  , toList
  ) where

import qualified Data.List               as List
import qualified Data.Map.Strict         as Map
import           Data.Setoid.Equivalence
import           Data.Setoid.Types
import           Prelude                 hiding (filter, lookup, map, mapM,
                                          mapM_, null, zip)
import qualified Prelude                 as P

-- | Instance Show, used for debugging
instance (Show a) =>
         Show (Setoid e a) where
  show s = "Setoid {\n" ++ P.unlines (P.map show (toList s)) ++ "}"

-- | Monoid instance for Setoid
instance (Ord e, Ord a) =>
         Monoid (Setoid e a) where
  mempty = empty
  mappend = union
  mconcat = unions

-- * Operators
infix 4 =~=

-- | Same as equivalence
(=~=)
  :: (Eq e)
  => Setoid e a -> Setoid e a -> Bool
(=~=) = equivalence

infix 5 \\

-- | Same as difference
(\\)
  :: (Ord e)
  => Setoid e a -> Setoid e a -> Setoid e a
(\\) = difference

-- | Same as union
(∪)
  :: (Ord e, Ord a)
  => Setoid e a -> Setoid e a -> Setoid e a
(∪) = union

-- * Construction
-- | An empty Setoid
empty :: Setoid e a
empty = Setoid Map.empty

-- | Same as empty
ø :: Setoid e a
ø = empty

-- | A Setoid with a single element
singleton
  :: (EquivalenceBy e a)
  => a -> Setoid e a
singleton a = Setoid (Map.singleton (eqRel a) a)

-- | Combine two Setoids resolving conflicts with `max` by
-- default. This makes the union operation symmetrical.
union
  :: (Ord e, Ord a)
  => Setoid e a -> Setoid e a -> Setoid e a
union = unionWith max

-- | A generalized variant of union which accepts a function that will
-- be used when two equivalent elements are found an the conflict
-- needs to be resolved. Note that the elements are not necessarily
-- equal
unionWith
  :: (Ord e)
  => (a -> a -> a) -> Setoid e a -> Setoid e a -> Setoid e a
unionWith f (Setoid x1) (Setoid x2) = Setoid (Map.unionWith f x1 x2)

-- | Union several Setoids into one. This uses de default union
-- variant
unions
  :: (Ord e, Ord a)
  => [Setoid e a] -> Setoid e a
unions = List.foldl' union empty

-- | Difference of two setoids. Return elements of the first setoid
-- not existing in the second setoid.
difference
  :: (Ord e)
  => Setoid e a -> Setoid e a -> Setoid e a
difference (Setoid x) (Setoid y) = Setoid (Map.difference x y)

-- * Conversion
-- | Test if an element is a member of a Setoid
member
  :: (EquivalenceBy e a, Ord e)
  => a -> Setoid e a -> Bool
member e (Setoid x) = Map.member (eqRel e) x

-- | Test if Setoid is empty
null :: Setoid e a -> Bool
null (Setoid x) = Map.null x

-- | Test if two Setoids are equivalent i.e. if all the elements are
-- equivalent
equivalence
  :: (Eq e)
  => Setoid e a -> Setoid e a -> Bool
equivalence (Setoid x) (Setoid y) = Map.keys x == Map.keys y

-- * Conversion
-- ** Lists
-- | Convert setoid into a List
toList :: Setoid e a -> [a]
toList (Setoid a) = Map.elems a

-- | A default variant of fromList using `max` to resolve a conflict
-- if two equivalent elements are found. Therefore it depends on Ord
-- instance of the element
fromList
  :: (EquivalenceBy e a, Ord e, Ord a)
  => [a] -> Setoid e a
fromList = fromListWith max

-- | A generalized version of fromList, which accepts a function
-- invoked if a conflict between two equivalent elements has to be
-- resolved
fromListWith
  :: (EquivalenceBy e a, Ord e)
  => (a -> a -> a) -> [a] -> Setoid e a
fromListWith f = Setoid . Map.fromListWith f . P.map (\x -> (eqRel x, x)) -- O(n+(n*log n)) -- An implementation of List.foldl' (\a b -> union a (singleton b)) empty would be O(n*2n)
