-- Which of the following can have a valid Functor instance

-- 1. data Bool = False | True
-- No, has kind *

-- 2. data BoolAndSomethingElse a = False' a | True' a
-- yes, has kind * -> *

-- 3. data BoolAndMaybeSomethingElse a = Falsish | Truish a
-- Yes, this is just Maybe in a different hat

-- 4. newtype Mu f = InF { outF :: f (Mu f) }
-- Intuition is yes, from kind of Mu being kind * -> *, but
-- if f can be f (Mu f) then the kind of f must be * -> *
-- so the kind of Mu is actually * -> (* -> *) which wouldn't be valid?
-- So I think No - no valid functor for Mu

-- 5.
--     import GHC.Arr data D =
--         D (Array Word Word) Int Int
-- No. Has kind *

{-# LANGUAGE FlexibleInstances #-}


data Sum b a = First a | Second b

instance Functor (Sum e) where
    fmap f (First a)  = First (f a)
    fmap f (Second b) = Second b

data Company a c b = DeepBlue a c | Something b

instance Functor (Company e e') where
    fmap f (Something b)  = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c

data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'

data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
    fmap f (Bloor x) = Bloor $ f x
    fmap _ (Desk x)  = Desk x
    fmap _ Finance   = Finance

data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
    fmap _ (K x) = K x


newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype D a b = D a

instance Functor (Flip D a) where
    fmap f (Flip (D x)) = Flip $ D $ f x

data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst x) = GoatyConst $ f x

data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut x) = LiftItOut $ fmap f x

data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa x y) = DaWrappa (fmap f x) (fmap f y)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)

instance Functor g => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething x y) = IgnoringSomething x (fmap f y)

data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious x y z) = Notorious x y (fmap f z)

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap f (Cons x y) = Cons (f x) (fmap f y)
    fmap _ Nil        = Nil

data GoatLord a =
      NoGoat
    | OneGoat a
    | MoreGoats (GoatLord a)
                (GoatLord a)
                (GoatLord a)
                deriving (Eq, Show)

instance Functor GoatLord where
    fmap _ NoGoat            = NoGoat
    fmap f (OneGoat x)       = OneGoat $ f x
    fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)


data TalkToMe a =
      Halt
    | Print String a
    | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt        = Halt
    fmap f (Print s x) = Print s (f x)
    fmap f (Read g)    = Read (f . g)
