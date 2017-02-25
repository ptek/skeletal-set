{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

--------------------------------------------------------------------
{- |
Module    : Data.Setoid
Copyright : (c) Global Access Internet Services GmbH
License   : BSD3
Maintainer: Pavlo Kerestey <pavlo@kerestey.net>

A Haskell implementation of
[setoid](https://en.wikipedia.org/wiki/Setoid) - a set equipped with
an equivalence relation. Setoid is a useful data structure when
equivalence is chosen not to be equality. This allows to influence the
membership of the elements in a setoid. When equality is all one needs
- using sets is a better option.

Here we have chosen to use a specific variant of equivalence of
transforming the elements to comparable intermediaries. Although it
does not make every equivalence relation possible, it is a practial
choice for a lot of computations.

== Usage

When manipulating collections of objects in the real world, we often
use lists/arrays. Sometimes we need to represent some properties of
the relation between the elements though, and the lists do not provide
such possibility. This library not only provides the guarantee that a
setoid is correct by construction, but also that the manipulations
will not change its structure.

We use it to run computations over time series of sampling data,
collections of users (who are unique by username or email) - to keep
the same structure as the one which would be used in the database with
unique indexes.

To implement equivalence we chose to use a data class `EquivalenceBy`
which provides a method of mapping an element to an intermediary,
which is then used for comparison and ultimately lead to a choice
of the members.

The type of a setoid is `Setoid e a` where `a` is the member type and
e is the type of equivalence intermediary. To chose the members of the
setoid we compare the e(quivalences) of the elements with each other.

The definition of `EquivalenceBy e a` is

@
class EquivalenceBy e a where
  eqRel :: a -> e
@

To give a simple example of how the library could be used we will
combine apples and oranges to a Setoid of fruit names by color. We
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

== Future Work

- There is an unproven hypothesis about a relation between setoids and
  Quotient Sets. It seems, that a `Setoid (a,b) (a,b,c)` is equivalent
  to a `QuotientSet a (Setoid b (a,b,c))`. This means that every
  QuotientSet can actually be represented as a setoid.

- Performance is another issue. Current implementation uses the
  `newtype Setoid x y = Setoid (Map x y)` which may be inefficient.


-}
--------------------------------------------------------------------
module Data.Setoid
  ( -- * Type
    Setoid
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
    -- * Filter
  , filter
    -- * Query
  , null
  , size
  , member
  , equivalence
    -- * Traversal
    -- ** map
  , map
  , mapResolve
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

-- ** Filter
-- | Filter a setoid. Return a setoid with elements that statisfy the
-- predicate
filter
  :: (Ord e)
  => (a -> Bool) -> Setoid e a -> Setoid e a
filter p (Setoid s) = Setoid (Map.filter p s)

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
-- | Map a function over elements of a setoid. It resolves conflict in
-- the result by chosing the maximum one
map
  :: (EquivalenceBy eb b, Ord eb, Ord b)
  => (a -> b) -> Setoid ea a -> Setoid eb b
map f a = fromList (P.map f (toList a))

-- | Generalized version of map, allowing to use custom function to
-- resolve a conflict if two equivalent elements are found in the
-- result
mapResolve
  :: (EquivalenceBy eb b, Ord eb)
  => (b -> b -> b) -- ^ conflict resolution function
  -> (a -> b)      -- ^ map function
  -> Setoid ea a   -- ^ input
  -> Setoid eb b   -- ^ result
mapResolve r f a = fromListWith r (P.map f (toList a))

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
