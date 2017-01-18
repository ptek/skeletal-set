{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SetoidTest where

import Data.Setoid             hiding (ø, (\\), (∪))
import Data.Setoid.Equivalence
import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck

test_basic_equality :: [TestTree]
test_basic_equality =
  [ testCase "empty" (ø @?= ø)
  , testProperty "not empty" (\x -> st x /= ø)
  , testProperty "equal to itself" (\x -> st x == st x)
  , testProperty "different" (\x y -> x /= y ==> st x /= st y)
  , testProperty "similar" (\x y -> x == y ==> st x == st y)
  , testProperty
      "transitive"
      (d500
         (over
            similar3
            (\(x, y, z) -> (x == y) && (y == z) ==> st (x, x) == st (z, z))))
  ]

test_union_laws :: [TestTree]
test_union_laws =
  [ testProperty "identity" (d3 (\a -> (a ∪ ø == a) && (ø ∪ a == a)))
  , testProperty "reflexivity" (d3 (\a -> a ∪ a == a))
  , testProperty "commutativity" (d3 (\a b -> a ∪ b == b ∪ a))
  , testProperty "associativity" (d3 (\a b c -> a ∪ (b ∪ c) == (a ∪ b) ∪ c))
  , testProperty "nonempty successor" (d3 (\a x -> a ∪ st x /= ø))
  ]

test_unionWith :: [TestTree]
test_unionWith =
  [ testProperty
      "unionWith max behaves the same as union"
      (d3 (\a b -> unionWith max a b == a ∪ b))
  ]

test_monoid_laws :: [TestTree]
test_monoid_laws =
  [ testProperty "associativity" (d3 (\a b c -> a <> (b <> c) == (a <> b) <> c))
  , testProperty "left identity" (d3 (\a -> a <> ø == a))
  , testProperty "right identity" (d3 (\a -> ø <> a == a))
  ]

test_difference :: [TestTree]
test_difference =
  [ testProperty "identity" (d3 (\a -> (a \\ ø == a) && (ø \\ a == ø)))
  , testProperty
      "definition"
      (d3
         (\a b ->
            all (\x -> (x `member` a) && not (x `member` b)) (toList (a \\ b))))
  ]

test_fromList :: [TestTree]
test_fromList =
  [ testCase "zero" (fromList [] @?= ø)
  , testProperty "singleton" (\x -> fromList [x] =~= st x)
  , testProperty
      "union eq"
      (d3 (\xs ys -> fromList (xs ++ ys) == fromList xs ∪ fromList ys))
  , testProperty
      "union equiv"
      (d3 (\xs ys -> fromList (xs ++ ys) =~= fromList ys ∪ fromList xs))
  , testProperty
      "union transitive"
      (d3
         (\xs ys zs ->
            fromList (xs ++ ys) ∪ fromList zs =~= fromList xs ∪
            fromList (ys ++ zs)))
  ]

test_fromListWith :: [TestTree]
test_fromListWith =
  [ testProperty
      "fromListWith max == fromList"
      (d3 (\xs -> fromListWith max xs == (fromList xs :: Setoid Int (Int, Int))))
  ]

test_toList :: [TestTree]
test_toList =
  [testProperty "inverse to fromList" (d3 (toList `isInverseOf` fromList))]

instance (Eq k, Ord k) =>
         EquivalenceBy k (k, v) where
  eqRel = fst

st
  :: EquivalenceBy Int (Int, Int)
  => (Int, Int) -> Setoid Int (Int, Int)
st = singleton

ø :: Setoid Int (Int, Int)
ø = empty

(<>) :: Setoid Int (Int, Int) -> Setoid Int (Int, Int) -> Setoid Int (Int, Int)
(<>) = mappend

(∪) :: Setoid Int (Int, Int) -> Setoid Int (Int, Int) -> Setoid Int (Int, Int)
(∪) = union

(\\) :: Setoid Int (Int, Int) -> Setoid Int (Int, Int) -> Setoid Int (Int, Int)
(\\) = difference

different2 :: Series m (Int, Int)
different2 = generate (\d -> [(k + 1, k + 2) | k <- [0 .. d]])

similar2 :: Series m (Int, Int)
similar2 = generate (\d -> [(k, k) | k <- [0 .. d]])

similar3 :: Series m (Int, Int, Int)
similar3 = generate (\d -> [(k, k, k) | k <- [0 .. d]])

isInverseOf
  :: (Setoid Int (Int, Int) -> a)
  -> (a -> Setoid Int (Int, Int))
  -> Setoid Int (Int, Int)
  -> Bool
isInverseOf f g a = (g . f) a == a

instance (Monad m, Ord v, Serial m v, EquivalenceBy k v) =>
         Serial m (Setoid k v) where
  series = fromList <$> series

d2
  :: Testable m a
  => a -> Property m
d2 = changeDepth (const 2)

d3
  :: Testable m a
  => a -> Property m
d3 = changeDepth (const 3)

d5
  :: Testable m a
  => a -> Property m
d5 = changeDepth (const 5)

d10
  :: Testable m a
  => a -> Property m
d10 = changeDepth (const 10)

d500
  :: Testable m a
  => a -> Property m
d500 = changeDepth (const 500)
