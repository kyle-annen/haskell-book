{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Chapter16 where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function

type S = String

type B = Bool

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorComposition :: (Functor f, Eq (f c)) => f a -> Fun a b -> Fun b c -> Bool
functorComposition x (Fun _ f) (Fun _ g) =
  fmap (g . f) x == (fmap g . fmap f $ x)

-- 975
data FixMePls a
  = FixMe
  | Pls a
  deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (FixMePls a) where
  arbitrary = do
    a <- arbitrary
    oneof [return $ Pls a, return FixMe]

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)

-- 16.6 982
data WhoCares a
  = ItDoesnt
  | Matter a
  | WhatThisIsCalled
  deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (WhoCares a) where
  arbitrary = do
    a <- arbitrary
    oneof [return $ ItDoesnt, return $ Matter a, return WhatThisIsCalled]

instance Functor WhoCares where
  fmap _ ItDoesnt = ItDoesnt
  fmap _ WhatThisIsCalled = WhatThisIsCalled
  fmap f (Matter a) = Matter (f a)

-- 16.10 Incances of Func
-- 1
newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

-- 2
data Pair a =
  Pair a
       a
  deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return $ Pair a a'

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

-- 3
data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

-- 4
data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

-- Quesiton: why does fmap only opperate on the last value in Three?
instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

-- 5
data Three' a b =
  Three' a
         b
         b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary -- QUESTION: why use b' here instead of c?
    return $ Three' a b b'

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

-- 6
data Four a b c d =
  Four a
       b
       c
       d
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance Functor (Four a b c) where
  fmap f (Four a b c c') = Four a b c (f c')

-- 7
data Four' a b =
  Four' a
        a
        a
        b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    a'' <- arbitrary
    b <- arbitrary
    return $ Four' a a' a'' b

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

-- Short Escercise 1023
-- 1
data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ First a, return $ Second b]

instance Functor (Sum a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)

-- 16.17 Chapter Excercises 1043
-- 1
data Quant a b
  = Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return Finance, return $ Desk a, return $ Bloor b]

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap f (Desk a) = Desk a
  fmap f (Bloor a) = Bloor (f a)

-- 2
newtype K a b =
  K a
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (K a b) where
  arbitrary = K <$> arbitrary

instance Functor (K a) where
  fmap _ (K a) = K a

-- 3
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance (Arbitrary b) => Arbitrary (Flip K a b) where
  arbitrary = do
    b <- arbitrary
    return $ Flip (K b)

instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip $ K (f b)

-- 4
data EvilGoateeConst a b =
  GoatyConst b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
  arbitrary = GoatyConst <$> arbitrary

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

spec :: SpecWith ()
spec = do
  describe "FixMePls" $ do
    it "abides by functor identity law" $
      property (functorIdentity :: FixMePls S -> B)
    it "abides by functor composition law" $
      property (functorComposition :: FixMePls S -> Fun S S -> Fun S S -> B)
  describe "WhoCares" $ do
    it "abides by functor identity law" $
      property (functorIdentity :: WhoCares S -> B)
    it "abides by functor composition law" $
      property (functorComposition :: WhoCares S -> Fun S S -> Fun S S -> B)
  describe "WhoCares" $ do
    it "abides by functor identity law" $
      property (functorIdentity :: Identity S -> B)
    it "abides by functor composition law" $
      property (functorComposition :: Identity S -> Fun S S -> Fun S S -> B)
  describe "Pair" $ do
    it "abides by functor identity law" $
      property (functorIdentity :: Pair S -> B)
    it "abides by functor composition law" $
      property (functorComposition :: Pair S -> Fun S S -> Fun S S -> B)
  describe "Two" $ do
    it "abides by functor identity law" $
      property (functorIdentity :: Two S S -> B)
    it "abides by functor composition law" $
      property (functorComposition :: Two S S -> Fun S S -> Fun S S -> B)
  describe "Three" $ do
    it "abides by functor identity law" $
      property (functorIdentity :: Three S S S -> B)
    it "abides by functor composition law" $
      property (functorComposition :: Three S S S -> Fun S S -> Fun S S -> B)
  describe "Three'" $ do
    it "abides by functor identity law" $
      property (functorIdentity :: Three' S S -> B)
    it "abides by functor composition law" $
      property (functorComposition :: Three' S S -> Fun S S -> Fun S S -> B)
  describe "Four" $ do
    it "abides by functor identity law" $
      property (functorIdentity :: Four S S S S -> B)
    it "abides by functor composition law" $
      property (functorComposition :: Four S S S S -> Fun S S -> Fun S S -> B)
  describe "Four'" $ do
    it "abides by functor identity law" $
      property (functorIdentity :: Four' S S -> B)
    it "abides by functor composition law" $
      property (functorComposition :: Four' S S -> Fun S S -> Fun S S -> B)
  describe "Sum" $ do
    it "abides by functor identity law" $
      property (functorIdentity :: Sum S S -> B)
    it "abides by functor composition law" $
      property (functorComposition :: Sum S S -> Fun S S -> Fun S S -> B)
  describe "Quant" $ do
    it "abides by functor identity law" $
      property (functorIdentity :: Quant S S -> B)
    it "abides by functor composition law" $
      property (functorComposition :: Quant S S -> Fun S S -> Fun S S -> B)
  describe "K" $ do
    it "abides by functor identity law" $
      property (functorIdentity :: K S S -> B)
    it "abides by functor composition law" $
      property (functorComposition :: K S S -> Fun S S -> Fun S S -> B)
  describe "EvilGoateeConst" $ do
    it "abides by functor identity law" $
      property (functorIdentity :: EvilGoateeConst S S -> B)
    it "abides by functor composition law" $
      property
        (functorComposition :: EvilGoateeConst S S -> Fun S S -> Fun S S -> B)
  describe "EvilGoateeConst" $ do
    it "abides by functor identity law" $
      property (functorIdentity :: EvilGoateeConst S S -> B)
    it "abides by functor composition law" $
      property
        (functorComposition :: EvilGoateeConst S S -> Fun S S -> Fun S S -> B)
