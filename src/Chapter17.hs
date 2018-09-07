{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Chapter17 where

import Control.Applicative (liftA3)
import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Data.List (elemIndex)

type S = String

type B = Bool

type I = Int

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (mempty <> a) == a

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (a <> mempty) == a

-- Excercises: Lookups
--1
added :: Maybe Integer
added = (+ 3) <$> lookup 3 (zip [1, 2, 3] [4, 5, 6])

--2
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

--3
xs = [1, 2, 3]

ys = [4, 5, 6]

x1 :: Maybe Integer
x1 = lookup 3 $ zip xs ys

y1 :: Maybe Integer
y1 = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = pure sum <*> ((,) <$> x1 <*> y1)

-- Identity
newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity $ f x

-- Constant
newtype Constant a b = Constant
  { getConstant :: a
  } deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure = const $ Constant mempty
  (Constant a) <*> (Constant a') = Constant $ a <> a'

-- Chapter Exercises
--1
data Pair a =
  Pair a
       a
  deriving (Show, Ord, Eq)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return $ Pair a a'

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a
  Pair f f' <*> Pair a a' = Pair (f a) (f' a')

-- 2
data Two a b =
  Two a
      b
  deriving (Eq, Ord, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

-- why does this only opperate on the last value?
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  Two a f <*> Two a' b = Two (a <> a') (f b)

-- 3
data Three a b c =
  Three a
        b
        c
  deriving (Eq, Ord, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Monoid a => Applicative (Three a a) where
  pure = Three mempty mempty
  Three a b f <*> Three a' b' c = Three (a <> a') (b <> b') (f c)

-- 4
data Three' a b =
  Three' a
         b
         b
  deriving (Eq, Ord, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Three' a b b'

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Monoid a => Applicative (Three' a) where
  pure a = Three' mempty a a
  Three' a f f' <*> Three' a' b b' = Three' (a <> a') (f b) (f' b')

-- 5
data Four a b c d =
  Four a
       b
       c
       d
  deriving (Eq, Ord, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a) => Applicative (Four a a a) where
  pure = Four mempty mempty mempty
  Four a b c f <*> Four a' b' c' d = Four (a <> a') (b <> b') (c <> c') (f d)

-- 6
data Four' a b =
  Four' a
        a
        a
        b
  deriving (Eq, Ord, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    a'' <- arbitrary
    b <- arbitrary
    return $ Four' a a' a'' b

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  Four' a b c f <*> Four' a' b' c' d = Four' (a <> a') (b <> b') (c <> c') (f d)

spec :: SpecWith ()
spec = do
  describe "Identity" $
    testBatch $ applicative (undefined :: Identity (Int, Int, Int))
  describe "Constant" $
    testBatch $ applicative (undefined :: Constant S (Int, Int, Int))
  describe "Pair" $ do
    testBatch $ functor (undefined :: Pair (Int, Int, Int))
    testBatch $ applicative (undefined :: Pair (Int, Int, Int))
  describe "Two" $ do
    testBatch $ functor (undefined :: Two S (Int, Int, Int))
    testBatch $ applicative (undefined :: Two S (Int, Int, Int))
  describe "Three" $ do
    testBatch $ functor (undefined :: Three S S (Int, Int, Int))
    testBatch $ applicative (undefined :: Three S S (Int, Int, Int))
  describe "Three'" $ do
    testBatch $ functor (undefined :: Three' S (Int, Int, Int))
    testBatch $ applicative (undefined :: Three' S (Int, Int, Int))
  describe "Four" $ do
    testBatch $ functor (undefined :: Four S S S (Int, Int, Int))
    testBatch $ applicative (undefined :: Four S S S (Int, Int, Int))
  describe "Four'" $ do
    testBatch $ functor (undefined :: Four' S (Int, Int, Int))
    testBatch $ applicative (undefined :: Four' S (Int, Int, Int))
