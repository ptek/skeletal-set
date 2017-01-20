{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.SetoidTest where

import Control.Monad.Identity hiding (mapM)
import Data.Setoid            hiding (ø, (\\), (∪))
import Prelude                hiding (map, mapM, null)
import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck

test_monoid_laws :: [TestTree]
test_monoid_laws =
  [ testProperty "associativity" (d3 (\a b c -> a <> (b <> c) == (a <> b) <> c))
  , testProperty "left identity" (d4 (\a -> a <> ø == a))
  , testProperty "right identity" (d4 (\a -> ø <> a == a))
  ]

test_construction :: [TestTree]
test_construction =
  [ testGroup "empty and singleton" $
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
  , testGroup "general union laws" $
    [ testProperty "identity" (d3 (\a -> (a ∪ ø == a) && (ø ∪ a == a)))
    , testProperty "reflexivity" (d3 (\a -> a ∪ a == a))
    , testProperty "commutativity" (d3 (\a b -> a ∪ b == b ∪ a))
    , testProperty "associativity" (d3 (\a b c -> a ∪ (b ∪ c) == (a ∪ b) ∪ c))
    , testProperty "nonempty successor" (d3 (\a x -> a ∪ st x /= ø))
    ]
  , testGroup "unionWith" $
    [ testProperty
        "unionWith max behaves the same as union"
        (d3 (\a b -> unionWith max a b == a ∪ b))
    ]
  , testGroup "unions" $
    [ testCase "zero" (unions [ø] @?= ø)
    , testProperty "one" (\a -> unions [st a] == st a)
    , testProperty "many" (d3 (\a b -> unions (a ++ [b]) == unions a ∪ b))
    ]
  , testGroup "difference" $
    [ testProperty "identity" (d3 (\a -> (a \\ ø == a) && (ø \\ a == ø)))
    , testProperty
        "by definition"
        (d3
           (\a b ->
              all (\x -> (x `member` a) && not (x `member` b)) (toList (a \\ b))))
    ]
  ]

test_queries :: [TestTree]
test_queries =
  [ testGroup "null" $
    [ testCase "empty" (null ø @?= True)
    , testProperty "singleton" (\a -> null (st a) == False)
    ]
  , testGroup "size" $
    [ testCase "empty" (size ø @?= 0)
    , testProperty "singleton" (\a -> size (st a) == 1)
    , testProperty
        "non emtpy"
        (d4 (\a -> size (a :: Setoid Int (Int, Int)) == length (toList a)))
    ]
  , testGroup "member" $
    [ testProperty "empty" (\x -> member x ø == False)
    , testProperty "single element" (\x -> member x (st x) == True)
    , testProperty "many elements" (d4 (\a x -> member x (a ∪ st x) == True))
    , testProperty "many elements" (d4 (\a x -> member x (a \\ st x) == False))
    ]
  , testGroup "equivalence" $
    [ testCase "empty case" (ø =~= ø @?= True)
    , testProperty
        "empty never equivalent to nonempty"
        (\x -> not (ø =~= (st x)) && not (st x =~= ø))
    , testProperty "singleton case" (\x -> st x =~= st x)
    , testProperty
        "not equivalent if eqRel is not equal"
        (over
           different
           (\(x, y) ->
              (eqRel x :: Int) /= (eqRel y :: Int) ==> not (st x =~= st y)))
    , testProperty
        "equivalent if eqRel is equal"
        (over
           similar
           (\(x, y) -> (eqRel x :: Int) == (eqRel y :: Int) ==> st x =~= st y))
    ]
  ]

test_traversal :: [TestTree]
test_traversal =
  [ testGroup "map" $
    [ testProperty "∃ x ∈ a: ∀ y ∈ (map f a): f x == y" $
      let f (x, y) = (x * y, x + y)
      in (d4 $ forAll $ \(a :: Setoid Int (Int, Int)) ->
            (`all` toList (map f a)) $ \y -> any (\x -> f x == y) (toList a))
    , testProperty "∀ x ∈ a: f x ∈ (map f a))" $
      let f (x, y) = (x * y, x + y)
      in (d4 $ forAll $ \(a :: Setoid Int (Int, Int)) ->
            (`all` toList a) $ \x -> f x `member` (map f a))
    ]
  , testGroup "mapM" $
    [ testProperty "is equivalent to pure version" $
      (\(f :: (Int, Int) -> (Int, Int)) (a :: Setoid Int (Int, Int)) ->
         mapM (return . f) a `mEqual` return (map f a))
    ]
  ]

test_conversion :: [TestTree]
test_conversion =
  [ testGroup "fromList" $
    [ testCase "zero" (fromList [] @?= ø)
    , testProperty "singleton" (\x -> fromList [x] =~= st x)
    , testProperty
        "via union"
        (d3 (\xs ys -> fromList (xs ++ ys) == fromList xs ∪ fromList ys))
    , testProperty
        "transitivity"
        (d3
           (\xs ys zs ->
              (fromList (xs ++ ys) ∪ fromList zs) =~=
              (fromList xs ∪ fromList (ys ++ zs))))
    ]
  , testGroup "fromListWith" $
    [ testProperty
        "fromListWith max == fromList"
        (d4
           (\xs -> fromListWith max xs == (fromList xs :: Setoid Int (Int, Int))))
    ]
  , testGroup "toList" $
    [testProperty "inverse to fromList" (d4 (toList `isInverseOf` fromList))]
  ]

instance EquivalenceBy k (k, v) where
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

different :: Series m ((Int, Int), (Int, Int))
different =
  generate (\d -> [((x - 1, y), (x + 1, y)) | x <- [0 .. d], y <- [0 .. 10]])

similar :: Series m ((Int, Int), (Int, Int))
similar =
  generate
    (\d -> [((x, y), (x, z)) | x <- [0 .. d], y <- [0 .. 5], z <- [4 .. 9]])

similar3 :: Series m (Int, Int, Int)
similar3 = generate (\d -> [(k, k, k) | k <- [0 .. d]])

isInverseOf
  :: (Setoid Int (Int, Int) -> a)
  -> (a -> Setoid Int (Int, Int))
  -> Setoid Int (Int, Int)
  -> Bool
isInverseOf f g a = (g . f) a == a

instance (Monad m, Ord k, Ord v, Serial m v, EquivalenceBy k v) =>
         Serial m (Setoid k v) where
  series = fromList <$> series

mEqual :: (Identity (Setoid Int (Int, Int)))
       -> (Identity (Setoid Int (Int, Int)))
       -> Bool
mEqual f g = runIdentity f == runIdentity g

d1
  :: Testable m a
  => a -> Property m
d1 = changeDepth (const 1)

d2
  :: Testable m a
  => a -> Property m
d2 = changeDepth (const 2)

d3
  :: Testable m a
  => a -> Property m
d3 = changeDepth (const 3)

d4
  :: Testable m a
  => a -> Property m
d4 = changeDepth (const 4)

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
