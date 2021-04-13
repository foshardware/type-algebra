{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Type.Algebra.Arithmetic where

import Control.Conditional
import Control.Lens
import Control.Monad.Fix
import Data.Bifunctor
import Data.Bifoldable
import Data.Profunctor
import Data.Void
import Data.Word

import Type.Algebra.Natural



class Haskell a b c d where haskell :: Iso a b c d


instance e ~ 0 => Haskell (T n) (T n) Void Void
  where haskell = iso (const (error "void")) (const 0)


instance n ~ 1 => Haskell (T n) (T n) () ()
  where haskell = iso (const ()) (const maxBound)

instance n ~ 2 => Haskell (T n) (T n) Bool Bool
  where haskell = iso (select even (const True) (const False)) (? bool maxBound minBound)


instance n ~ Power 2 8 => Haskell (T n) (T n) Word8 Word8
  where haskell = iso (fromIntegral . pred . toInteger) (fromInteger . succ . fromIntegral)

instance n ~ Power 2 16 => Haskell (T n) (T n) Word16 Word16
  where haskell = iso (fromIntegral . pred . toInteger) (fromInteger . succ . fromIntegral)

instance n ~ Power 2 32 => Haskell (T n) (T n) Word32 Word32
  where haskell = iso (fromIntegral . pred . toInteger) (fromInteger . succ . fromIntegral)

instance n ~ Power 2 64 => Haskell (T n) (T n) Word64 Word64
  where haskell = iso (fromIntegral . pred . toInteger) (fromInteger . succ . fromIntegral)


instance Haskell (a ^ b) (c ^ d) (b -> a) (d -> c)
  where haskell = iso unE E

instance Haskell (a + b) (c + d) (Either a b) (Either c d)
  where haskell = iso unA A


instance n ~ 1 => Haskell (T n + a) (T n + b) (Maybe a) (Maybe b)
  where haskell = iso (either (const Nothing) Just . unA) (A . maybe (Left maxBound) Right)

instance n ~ 1 => Haskell (a + T n) (b + T n) (Maybe a) (Maybe b)
  where haskell = iso (either Just (const Nothing) . unA) (A . maybe (Right maxBound) Left)


instance Haskell (a × b) (c × d) (a, b) (c, d)
  where haskell = iso unM M


-- probably not
-- instance Haskell a b a b
--   where haskell = iso id id


infixr 2 |-
class a |- b where sequent :: a -> b


instance n ~ 0 => a + T n |- a
  where sequent = either id (views haskell absurd) . unA

instance n ~ 0 => T n + a |- a
  where sequent = either (views haskell absurd) id . unA


instance n ~ 0 => a - T n |- a
  where sequent = minuend


instance n ~ 0 => a × T n |- T n
  where sequent = const maxBound

instance n ~ 0 => T n × a |- T n
  where sequent = const maxBound


instance n ~ 1 => a × T n |- a
  where sequent = fst . unM

instance n ~ 1 => T n × a |- a
  where sequent = snd . unM


instance n ~ 1 => a / T n |- a
  where sequent = enumerator

instance n ~ 1 => a / a |- T n
  where sequent = const maxBound

-- 0^0 = 1
instance (e ~ 0, n ~ 1) => a ^ T e |- T n
  where sequent = const maxBound

instance (e ~ 0, n ~ 1) => T e . a |- T n
  where sequent = const maxBound

instance n ~ 1 => a ^ T n |- a
  where sequent = flip unE maxBound

instance n ~ 1 => T n . a |- a
  where sequent = flip unC maxBound


instance a . b |- b ^ a
  where sequent = E . unC


instance (Natural n, n ~ Sum a b) => T a + T b |- T n
  where sequent = const maxBound

instance (Natural n, n ~ Product a b) => T a × T b |- T n
  where sequent = const maxBound

instance (Natural n, b <= a, n ~ Difference a b) => T a - T b |- T n
  where sequent = const maxBound

instance (Natural n, n /= 0, n ~ Quotient a b) => T a / T b |- T n
  where sequent = const maxBound


instance a × (b / a) |- b
  where sequent = enumerator . snd . unM

instance (a × b) / a |- b
  where sequent = snd . unM . enumerator


instance a + (b - a) |- b
  where sequent = sequent . bimap (const (T 0)) minuend

instance (a + b) - a |- b
  where sequent = sequent . first (const (T 0)) . minuend




-- should be an iso
productRule :: c ^ a × c ^ b -> c ^ (a + b)
productRule = undefined

-- should be an iso
powerRule :: c ^ b ^ a -> c ^ (a × b)
powerRule = undefined

-- should be an iso
quotientRule :: c ^ a / c ^ b -> c ^ (a - b)
quotientRule = undefined



infixr 8 ^
newtype a ^ b = E { unE :: b -> a }

instance Contravariant ((^) a)
  where contramap f (E g) = E (g . f)


-- dual to exponentation
infixr 9 .
newtype a . b = C { unC :: a -> b }
  deriving (Functor)
  deriving (Profunctor, Strong, Choice, Costrong) via (->)
  deriving (Applicative, Monad, MonadFix) via (->) a



infixl 6 +
newtype a + b = A { unA :: Either a b }
  deriving (Eq, Ord, Functor, Foldable, Traversable)
  deriving (Bifunctor, Bifoldable) via Either
  deriving (Applicative, Monad, MonadFix) via Either a
  deriving (Semigroup) via Either a b


infixl 7 ×
newtype a × b = M { unM :: (a, b) }
  deriving (Eq, Ord, Functor, Foldable, Traversable)
  deriving (Bifunctor, Bifoldable) via (,)
  deriving (Applicative, Monad) via (,) a
  deriving (Semigroup, Monoid) via (a, b)



class s <~ t where subtype :: t -> t - s

instance (Natural s, Natural t, s < t) => T s <~ T t
  where
    subtype = S <$> select (< k) (Just . fromIntegral) (const Nothing) <*> id
      where k = fromIntegral (maxBound :: T s) :: T t
  

infixl 6 -
data a - b = S (Maybe b) a
  deriving Functor

instance Bifunctor (-)
  where bimap f g (S b a) = S (g <$> b) (f a)

instance Bifoldable (-)
  where bifoldMap f g (S b a) = f a <> foldMap g b


instance Eq a => Eq (a - b)
  where f == g = deduct f == deduct g

instance Ord a => Ord (a - b)
  where f `compare` g = deduct f `compare` deduct g


-- ensures equality relation
difference :: a -> (a -> Maybe b) -> a - b
difference a f = S (f a) a


minuend :: a - b -> a
minuend (S _ a) = a

subtrahend :: a - b -> Maybe b
subtrahend (S b _) = b

-- substitute b with -1
deduct :: a - b -> Maybe a
deduct (S b a) = maybe (Just a) (const Nothing) b



infixl 7 /
data a / b = Q b a
  deriving Functor

instance Bifunctor (/)
  where bimap f g (Q b a) = Q (g b) (f a)

instance Bifoldable (/)
  where bifoldMap f g (Q b a) = f a <> g b


instance (Eq b, Natural n, n ~ 1) => Eq (a / b ^ T n)
  where Q f _ == Q g _ = unE f maxBound == unE g maxBound

instance Eq b => Eq (a / b ^ a)
  where Q f x == Q g y = unE f x == unE g y


instance (Ord b, Natural n, n ~ 1) => Ord (a / b ^ T n)
  where Q f _ `compare` Q g _ = unE f maxBound `compare` unE g maxBound

instance Ord b => Ord (a / b ^ a)
  where Q f x `compare` Q g y = unE f x `compare` unE g y


-- ensures equality relation
quotient :: a -> (a -> b) -> a / b
quotient a f = Q (f a) a


enumerator :: a / b -> a
enumerator (Q _ a) = a

denominator :: a / b -> b
denominator (Q b _) = b

