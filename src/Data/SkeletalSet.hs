{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

--------------------------------------------------------------------
{- |
Module    : Data.SkeletalSet
Copyright : (c) Global Access Internet Services GmbH
License   : BSD3
Maintainer: Pavlo Kerestey <pavlo@kerestey.net>

A Haskell implementation of skeletal set - a set equipped with an equivalence
relation. SkeletalSet is a useful data structure when equivalence is chosen not
to be equality. This allows to influence the membership of the elements in a
set.

Here we have chosen to use a specific variant of equivalence of transforming the
elements to comparable intermediaries. Although it does not make every
equivalence relation possible, it is a practical choice for a lot of
computations.

== Usage

When manipulating collections of objects in the real world, we often use
lists/arrays. Sometimes we need to represent some properties of the relation
between the elements though, and the lists do not provide such possibility. This
library not only provides the guarantee that a skeletal set is correct by
construction, but also that the manipulations will not change its structure.

We use it to run computations over time series of sampling data, collections of
users (who are unique by username or email) - to keep the same structure as the
one which would be used in the database with unique indexes.

To implement equivalence we chose to use a data class `EquivalenceBy` which
provides a method of mapping an element to an intermediary, which is then used
for comparison and ultimately lead to a choice of the members.

The type is `SkeletalSet e a` where `a` is the member type and e is the type of
equivalence intermediary. To chose the members of the skeletal set we compare
the e(quivalences) of the elements with each other.

The definition of `EquivalenceBy e a` is

@
class EquivalenceBy e a where
  eqRel :: a -> e
@

To give a simple example of how the library could be used we will combine apples
and oranges to a SkeletalSet of fruit names by colour. We want one fruit per
colour as a result and don't care if its apple or an orange.

@
import Data.SkeletalSet (SkeletalSet)
import qualified Data.SkeletalSet as SkeletalSet

data Colour = Red | Green | Blue deriving (Eq,Ord)

instance EquivalenceBy Colour (Colour,String) where
  eqRel = fst

apples, organges, fruits :: SkeletalSet Int (Int,String)
apples  = SkeletalSet.fromList [(Green,"golden delicious"), (Orange,"honeycrunch")]
oranges = SkeletalSet.fromList [(Orange,"seville"), (Red,"blood orange")]

fruits = apples `SkeletalSet.union` oranges
-- > [(Green,"golden delicious"), (Orange,"seville"), (Red,"blood orange")]
@

One can see the benefit of using a `SkeletalSet` instead of "Data.List" because
with the latter, we would have to use 'Data.List.nubBy' every time the data is
transformed.

When performing a `union`, our implementation would use `max` between two
equivalent elements to resolve the conflict. Bear in mind, that the elements,
though equivalent, might not be equal. In the example above, ordering of @
"seville" @ is bigger than @ "golden delicious" @ thus @ ("Orange", "seville") @
is chosen in the result.

=== Friends of friends and computation on union

For another example, lets get all the users of two different services F and G.
We are not interested in the different details, but want the instance of the
users to be unique.

@
type Email = String
data User = User {
  email :: Email,
  contacts :: Int
  } deriving (Eq,Show)

instance EquivalenceBy Email User where
eqRel u = email u

usersF, usersG, allUsers :: SkeletalSet Email User
usersF <- getUsers F
usersG <- getUsers G

allUsers = SkeletalSet.unionWith mergeContactDetails usersF usersG

mergeContactDetails :: User -> User -> User
mergeContactDetails a b = User (email a) (contacts a + contacts b)
-- ... --
@

We assume that here are equivalent elements in both sets - in this case they
have the same email address. Thus we use `unionWith` to merge the other details
of the contact. Here, we could also do computations and, for example, sum the
number of friends/contacts from both services.

Here is also one of the shortcomings of the library. mergeContactDetails choses
the email of the first argument. Since in the context of unionWith, the emails
of the first and the second users are the same. It is not nice from the
perspective of the function itself though.

@ SkeletalSet.size allUsers @ Would give us the amount of all unique users in
both services together.

== Future Work

- There is an unproven hypothesis about a relation between skeletal sets and
  Quotient Sets. It seems, that a `SkeletalSet (a,b) (a,b,c)` is equivalent
  to a `QuotientSet a (SkeletalSet b (a,b,c))`. This means that every
  QuotientSet can actually be represented as a skeletal set.

- Performance is another issue. Current implementation uses the
  `newtype SkeletalSet x y = SkeletalSet (Map x y)` which may be inefficient.


-}
--------------------------------------------------------------------
module Data.SkeletalSet
  ( -- * Type
    SkeletalSet
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
  )
where

import qualified Data.List                     as List
import qualified Data.Map.Strict               as Map
import           Data.SkeletalSet.Equivalence
import           Data.SkeletalSet.Types
import           Prelude                 hiding ( filter
                                                , map
                                                , mapM
                                                , null
                                                )
import qualified Prelude                       as P

-- | Instance Show, used for debugging
instance (Show a) => Show (SkeletalSet e a) where
  show s = "{{ " ++ P.unlines (P.map show (toList s)) ++ " }}"

-- | Monoid instance for SkeletalSet
instance (Ord e, Ord a) => Monoid (SkeletalSet e a) where
  mempty = empty
  mappend = union
  mconcat = unions

-- * Operators
infix 4 =~=

-- | Same as `equivalence`
(=~=) :: (Eq e) => SkeletalSet e a -> SkeletalSet e a -> Bool
(=~=) = equivalence

infix 5 \\

-- | Same as `difference`
(\\) :: (Ord e) => SkeletalSet e a -> SkeletalSet e a -> SkeletalSet e a
(\\) = difference

-- | Same as `union`
(∪) :: (Ord e, Ord a) => SkeletalSet e a -> SkeletalSet e a -> SkeletalSet e a
(∪) = union

-- * Construction
-- | An empty SkeletalSet
empty :: SkeletalSet e a
empty = SkeletalSet Map.empty

-- | Same as `empty`
ø :: SkeletalSet e a
ø = empty

-- | A SkeletalSet with a single element
singleton :: (EquivalenceBy e a) => a -> SkeletalSet e a
singleton a = SkeletalSet (Map.singleton (eqRel a) a)

-- ** Combining
-- | Combine two SkeletalSets resolving conflicts with `max` by
-- default. This makes the union operation commutative and
-- associative.
union :: (Ord e, Ord a) => SkeletalSet e a -> SkeletalSet e a -> SkeletalSet e a
union = unionWith max

-- | A generalized variant of union which accepts a function that will
-- be used when two equivalent elements are found an the conflict
-- needs to be resolved. Note that the elements are not necessarily
-- equal
unionWith :: (Ord e)
          => (a -> a -> a)
          -> SkeletalSet e a
          -> SkeletalSet e a
          -> SkeletalSet e a
unionWith f (SkeletalSet x1) (SkeletalSet x2) =
  SkeletalSet (Map.unionWith f x1 x2)

-- | Union several SkeletalSets into one. This uses de default union
-- variant
unions :: (Ord e, Ord a) => [SkeletalSet e a] -> SkeletalSet e a
unions = List.foldl' union empty

-- ** Difference
-- | Difference of two skeletal sets. Return elements of the first skeletal sets
-- not existing in the second set.
difference :: (Ord e) => SkeletalSet e a -> SkeletalSet e a -> SkeletalSet e a
difference (SkeletalSet x) (SkeletalSet y) = SkeletalSet (Map.difference x y)

-- ** Filter
-- | Filter a skeletal set. Return a skeletal set with elements that statisfy the
-- predicate
filter :: (Ord e) => (a -> Bool) -> SkeletalSet e a -> SkeletalSet e a
filter p (SkeletalSet s) = SkeletalSet (Map.filter p s)

-- * Query
-- | Test if SkeletalSet is empty
null :: SkeletalSet e a -> Bool
null (SkeletalSet x) = Map.null x

-- | Get the size of a skeletal set
size :: SkeletalSet e a -> Int
size (SkeletalSet x) = Map.size x

-- | Test if an element is a member of a skeletal set
member :: (EquivalenceBy e a, Ord e) => a -> SkeletalSet e a -> Bool
member e (SkeletalSet x) = Map.member (eqRel e) x

-- | Test if two SkeletalSets are equivalent i.e. if all the elements are
-- equivalent
equivalence :: (Eq e) => SkeletalSet e a -> SkeletalSet e a -> Bool
equivalence (SkeletalSet x) (SkeletalSet y) = Map.keys x == Map.keys y

-- * Traversal
-- | Map a function over elements of a skeletal set. It resolves conflict in
-- the result by chosing the maximum one
map :: (EquivalenceBy eb b, Ord eb, Ord b)
    => (a -> b)
    -> SkeletalSet ea a
    -> SkeletalSet eb b
map f a = fromList (P.map f (toList a))

-- | Generalized version of map, allowing to use custom function to
-- resolve a conflict if two equivalent elements are found in the
-- result
mapResolve :: (EquivalenceBy eb b, Ord eb)
           => (b -> b -> b) -- ^ conflict resolution function
           -> (a -> b)      -- ^ map function
           -> SkeletalSet ea a   -- ^ input
           -> SkeletalSet eb b   -- ^ result
mapResolve r f a = fromListWith r (P.map f (toList a))

-- | Monadic variant of a map
mapM :: (Monad m, EquivalenceBy eb b, Ord eb, Ord b)
     => (a -> m b)
     -> SkeletalSet ea a
     -> m (SkeletalSet eb b)
mapM f xs = fromList <$> P.mapM f (toList xs)

-- * Conversion
-- ** Lists
-- | Convert skeletal set into a List
toList :: SkeletalSet e a -> [a]
toList (SkeletalSet a) = Map.elems a

-- | A default variant of fromList using `max` to resolve a conflict
-- if two equivalent elements are found. Therefore it depends on Ord
-- instance of the element
fromList :: (EquivalenceBy e a, Ord e, Ord a) => [a] -> SkeletalSet e a
fromList = fromListWith max

-- | A generalized version of fromList, which will use a supplied
-- funtion if two equivalent elements are found in the input list
fromListWith :: (EquivalenceBy e a, Ord e)
             => (a -> a -> a)
             -> [a]
             -> SkeletalSet e a
fromListWith f = SkeletalSet . Map.fromListWith f . P.map (\x -> (eqRel x, x))
-- O(n+(n*log n))
-- An implementation of List.foldl' (\a b -> union a (singleton b)) empty
-- would be O(n*2n)
