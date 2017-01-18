module Data.Setoid
  ( Setoid
  , (=~=)
  , (\\)
  , (∪)
  , difference
  , empty
  , singleton
  , member
  , null
  , union
  , unions
  , unionWith
  , ø
  , fromList
  , fromListWith
  , toList
  ) where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Setoid.Equivalence
import qualified Prelude as P
import Prelude hiding (filter, lookup, map, mapM, mapM_, null, zip)

data Setoid k a =
  Setoid !(Map k a)
  deriving (Eq, Ord)

instance (Show a) =>
         Show (Setoid k a) where
  show s = "Setoid {\n" ++ P.unlines (P.map show (toList s)) ++ "}"

instance (EquivalenceBy k a, Ord a) =>
         Monoid (Setoid k a) where
  mempty = empty
  mappend = union
  mconcat = unions

equivalence
  :: (EquivalenceBy k a)
  => Setoid k a -> Setoid k a -> Bool
equivalence (Setoid x) (Setoid y) = Map.keys x == Map.keys y

infix 4 =~=

(=~=)
  :: (EquivalenceBy k a)
  => Setoid k a -> Setoid k a -> Bool
(=~=) = equivalence

singleton
  :: (EquivalenceBy k a)
  => a -> Setoid k a
singleton a = Setoid (Map.singleton (eqRel a) a)

difference
  :: (Ord k)
  => Setoid k a -> Setoid k a -> Setoid k a
difference (Setoid x) (Setoid y) = Setoid (Map.difference x y)

infix 5 \\

(\\)
  :: (Ord k)
  => Setoid k a -> Setoid k a -> Setoid k a
(\\) = difference

empty :: Setoid k a
empty = Setoid Map.empty

union
  :: (EquivalenceBy k a, Ord a)
  => Setoid k a -> Setoid k a -> Setoid k a
union = unionWith max

(∪)
  :: (EquivalenceBy k a, Ord a)
  => Setoid k a -> Setoid k a -> Setoid k a
(∪) = union

ø :: Setoid k a
ø = empty

unionWith
  :: (EquivalenceBy k a)
  => (a -> a -> a) -> Setoid k a -> Setoid k a -> Setoid k a
unionWith f (Setoid x1) (Setoid x2) = Setoid (Map.unionWith f x1 x2)

unions
  :: (EquivalenceBy k a, Ord a)
  => [Setoid k a] -> Setoid k a
unions = List.foldl' union empty

toList :: Setoid k a -> [a]
toList (Setoid a) = Map.elems a

fromList
  :: (EquivalenceBy k a, Ord a)
  => [a] -> Setoid k a
fromList = fromListWith max

-- fromList = List.foldl' (\a b -> union a (singleton b)) empty -- O(n*2n)
fromListWith
  :: (EquivalenceBy k a)
  => (a -> a -> a) -> [a] -> Setoid k a
fromListWith f = Setoid . Map.fromListWith f . P.map (\x -> (eqRel x, x)) -- O(n+(n*log n))

member
  :: (EquivalenceBy k a)
  => a -> Setoid k a -> Bool
member e (Setoid x) = Map.member (eqRel e) x

null :: Setoid k a -> Bool
null (Setoid x) = Map.null x
