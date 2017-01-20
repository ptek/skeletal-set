{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

--------------------------------------------------------------------
{- |
Module    : Data.Setoid
Copyright : (c) Global Access Internet Services GmbH
License   : BSD3
Maintainer: Pavlo Kerestey <pavlo@kerestey.net>

This is a Haskell implementation of setoid
(https://en.wikipedia.org/wiki/Setoid) - a set equipped with an
equivalence relation. Mostly, one would chose equivalence to be not
the same as equality. This makes it more strict regarding membership
of elements as compared to sets. If equivalence relation is equality,
though, then setoid has the same properties as a set.

== Usage

To give a simple example, lets try out a somewhat obscure idea of
combining apples and oranges into a Setoid of fruit names (by color). We
want one fruit per colour as a result and don't care if its apple or
an orange.

@
import Data.Setoid (Setoid)
import qualified Data.Setoid as Setoid

data Colour = Red | Green | Blue deriving (Eq,Ord)

instance EquivalenceBy Colour (Colour,String) where
  eqRel = fst

apples, organges, fruits :: Setoid Int (Int,String)
apples  = Setoid.fromList [(Green,"golden delicious"), (Orange,"honeycrunch")]
oranges = Setoid.fromList [(Orange,"seville"), (Red,"blood orange")]

fruits = apples `Setoid.union` oranges
-- > [(Green,"golden delicious"), (Orange,"seville"), (Red,"blood orange")]
@

One can see the benefit of using a `Setoid` instead of "Data.List"
because with the latter, we would have to use 'Data.List.nubBy' every
time the data is transformed.

When performing a `union`, our implementation would use `max` between
two equivalent elements to resolve the conflict. Bear in mind, that
the elements, though equivalent, might not be equal. In the example
above, ordering of @ "seville" @ is bigger than @ "golden delicious" @
thus @ ("Orange", "seville") @ is chosen in the result.

=== Friends of friends and computation on union

For another example, lets get all the users of two different services
F and G. We are not interested in the different details, but want the
instance of the users to be unique.

@
type Email = String
data User = User {
  email :: Email,
  contacts :: Int
  } deriving (Eq,Show)

instance EquivalenceBy Email User where
eqRel u = email u

usersF, usersG, allUsers :: Setoid Email User
usersF <- getUsers F
usersG <- getUsers G

allUsers = Setoid.unionWith mergeContactDetails usersF usersG

mergeContactDetails :: User -> User -> User
mergeContactDetails a b = User (email a) (contacts a + contacts b)
-- ... --
@

We assume that here are equivalent elements in both setoids - in this
case they have the same email adress. Thus we use `unionWith` to merge
the other details of the contact. Here, we could also do computations
and, for example, sum the number of friends/contacts from bothe
services.

Here is also one of the shortcommings of the
library. mergeContactDetails choses the email of the first
argument. Sinse in the context of unionWith, the emails of the first
and the second users are the same. It is not nice from the perspective
of the function itself though.

@ Setoid.size allUsers @ Would give us the amount of all unique users
in both services together.

-}
--------------------------------------------------------------------
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
  , null
  , size
  , member
  , equivalence
    -- * Traversal
    -- ** map
  , map
  , mapM
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
  show s = "{{ " ++ P.unlines (P.map show (toList s)) ++ " }}"

-- | Monoid instance for Setoid
instance (Ord e, Ord a) =>
         Monoid (Setoid e a) where
  mempty = empty
  mappend = union
  mconcat = unions

-- * Operators
infix 4 =~=

-- | Same as `equivalence`
(=~=)
  :: (Eq e)
  => Setoid e a -> Setoid e a -> Bool
(=~=) = equivalence

infix 5 \\

-- | Same as `difference`
(\\)
  :: (Ord e)
  => Setoid e a -> Setoid e a -> Setoid e a
(\\) = difference

-- | Same as `union`
(∪)
  :: (Ord e, Ord a)
  => Setoid e a -> Setoid e a -> Setoid e a
(∪) = union

-- * Construction
-- | An empty Setoid
empty :: Setoid e a
empty = Setoid Map.empty

-- | Same as `empty`
ø :: Setoid e a
ø = empty

-- | A Setoid with a single element
singleton
  :: (EquivalenceBy e a)
  => a -> Setoid e a
singleton a = Setoid (Map.singleton (eqRel a) a)

-- ** Combining
-- | Combine two Setoids resolving conflicts with `max` by
-- default. This makes the union operation commutative and
-- associative.
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

-- ** Difference
-- | Difference of two setoids. Return elements of the first setoid
-- not existing in the second setoid.
difference
  :: (Ord e)
  => Setoid e a -> Setoid e a -> Setoid e a
difference (Setoid x) (Setoid y) = Setoid (Map.difference x y)

-- * Query
-- | Test if Setoid is empty
null :: Setoid e a -> Bool
null (Setoid x) = Map.null x

-- | Get the size of a setoid
size :: Setoid e a -> Int
size (Setoid x) = Map.size x

-- | Test if an element is a member of a setoid
member
  :: (EquivalenceBy e a, Ord e)
  => a -> Setoid e a -> Bool
member e (Setoid x) = Map.member (eqRel e) x

-- | Test if two Setoids are equivalent i.e. if all the elements are
-- equivalent
equivalence
  :: (Eq e)
  => Setoid e a -> Setoid e a -> Bool
equivalence (Setoid x) (Setoid y) = Map.keys x == Map.keys y

-- * Traversal
-- | Map a function over elements of a setoid
map
  :: (EquivalenceBy eb b, Ord eb, Ord b)
  => (a -> b) -> Setoid ea a -> Setoid eb b
map f = fromList . P.map f . toList

-- | Monadic variant of a map
mapM
  :: (Monad m, EquivalenceBy eb b, Ord eb, Ord b)
  => (a -> m b) -> Setoid ea a -> m (Setoid eb b)
mapM f xs = fromList <$> P.mapM f (toList xs)

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

-- | A generalized version of fromList, which will use a supplied
-- funtion if two equivalent elements are found in the input list
fromListWith
  :: (EquivalenceBy e a, Ord e)
  => (a -> a -> a) -> [a] -> Setoid e a
fromListWith f = Setoid . Map.fromListWith f . P.map (\x -> (eqRel x, x))
-- O(n+(n*log n))
-- An implementation of List.foldl' (\a b -> union a (singleton b)) empty would be O(n*2n)
