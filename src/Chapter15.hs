module Chapter15 where

import Data.Functor
import Data.Monoid
import Test.Hspec
import Test.QuickCheck hiding (Failure, Success)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (mempty <> a) == a

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (a <> mempty) == a

data Trivial =
  Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

instance Monoid Trivial where
  mempty = Trivial

type S = String

type B = Bool

-- Identity
data Identity a =
  Identity a
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Semigroup a => Semigroup (Identity a) where
  Identity a <> Identity b = Identity $ a <> b

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

-- Two
data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

data One a =
  One a
  deriving (Eq, Show)

-- Three
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

instance (Semigroup a, Semigroup b, Semigroup c) =>
         Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty = Three mempty mempty mempty

-- Four
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

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
         Semigroup (Four a b c d) where
  (Four a b c d) <> (Four a' b' c' d') =
    Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
  mempty = Four mempty mempty mempty mempty

-- BoolConj
data BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    return $ BoolConj a

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  _ <> _ = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True

-- BoolConj
data BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary
    return $ BoolDisj a

instance Semigroup BoolDisj where
  BoolDisj False <> BoolDisj False = BoolDisj False
  _ <> _ = BoolDisj True

instance Monoid BoolDisj where
  mempty = BoolDisj False

-- Or
data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Fst a, return $ Snd b]

instance Semigroup (Or a b) where
  Snd a <> _ = Snd a
  _ <> Snd b = Snd b
  _ <> a = a

spec :: SpecWith ()
spec = do
  describe "Trivial" $ do
    it "obeys associativity" $
      property (semigroupAssoc :: Trivial -> Trivial -> Trivial -> B)
    it "obeys right identity" $ property (monoidRightIdentity :: Trivial -> B)
    it "obeys left identity" $ property (monoidLeftIdentity :: Trivial -> B)
  describe "Identity" $ do
    it "obeys associativity" $
      property (semigroupAssoc :: Identity S -> Identity S -> Identity S -> B)
    it "obeys right identity" $
      property (monoidRightIdentity :: Identity S -> B)
    it "obeys left identity" $ property (monoidLeftIdentity :: Identity S -> B)
  describe "Two" $ do
    it "obeys associativity" $
      property (semigroupAssoc :: Two S S -> Two S S -> Two S S -> B)
    it "obeys right identity" $ property (monoidRightIdentity :: Two S S -> B)
    it "obeys left identity" $ property (monoidLeftIdentity :: Two S S -> B)
  describe "Three" $ do
    it "obeys associativity" $
      property
        (semigroupAssoc :: Three S S S -> Three S S S -> Three S S S -> B)
    it "obeys right identity" $
      property (monoidRightIdentity :: Three S S S -> B)
    it "obeys left identity" $ property (monoidLeftIdentity :: Three S S S -> B)
  describe "Four" $ do
    it "obeys associativity" $
      property
        (semigroupAssoc :: Four S S S S -> Four S S S S -> Four S S S S -> B)
    it "obeys right identity" $
      property (monoidRightIdentity :: Four S S S S -> B)
    it "obeys left identity" $
      property (monoidLeftIdentity :: Four S S S S -> B)
  describe "BoolConj" $ do
    it "obeys associativity" $
      property (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> B)
    it "obeys right identity" $ property (monoidRightIdentity :: BoolConj -> B)
    it "obeys left identity" $ property (monoidLeftIdentity :: BoolConj -> B)
    it "for True/True" $ BoolConj True <> BoolConj True `shouldBe` BoolConj True
    it "for True/False" $
      BoolConj True <> BoolConj False `shouldBe` BoolConj False
    it "for False/True" $
      BoolConj False <> BoolConj True `shouldBe` BoolConj False
    it "for False/False" $
      BoolConj False <> BoolConj False `shouldBe` BoolConj False
  describe "BoolDisj" $ do
    it "obeys associativity" $
      property (semigroupAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> B)
    it "obeys right identity" $ property (monoidRightIdentity :: BoolDisj -> B)
    it "obeys left identity" $ property (monoidLeftIdentity :: BoolDisj -> B)
    it "for True/True" $ BoolDisj True <> BoolDisj True `shouldBe` BoolDisj True
    it "for True/False" $
      BoolDisj True <> BoolDisj False `shouldBe` BoolDisj True
    it "for False/True" $
      BoolDisj False <> BoolDisj True `shouldBe` BoolDisj True
    it "for False/False" $
      BoolDisj False <> BoolDisj False `shouldBe` BoolDisj False
  describe "Or" $ do
    it "obeys associativity" $
      property (semigroupAssoc :: Or S S -> Or S S -> Or S S -> B)
    it "Fst/Snd" $ Fst 1 <> Snd 2 `shouldBe` (Snd 2 :: Or Int Int)
    it "Fst/Fst" $ Fst 1 <> Fst 2 `shouldBe` (Fst 2 :: Or Int Int)
    it "Fst/Snd" $ Snd 1 <> Fst 2 `shouldBe` (Snd 1 :: Or Int Int)
    it "Fst/Snd" $ Snd 1 <> Snd 2 `shouldBe` (Snd 1 :: Or Int Int)
