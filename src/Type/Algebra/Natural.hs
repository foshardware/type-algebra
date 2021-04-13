{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}

module Type.Algebra.Natural where

import Data.Proxy
import qualified GHC.Base as GHC
import qualified GHC.TypeNats as GHC


class GHC.KnownNat n => Natural n

type Nat = GHC.Nat


newtype T (n :: Nat) = T Integer
  deriving (Eq, Ord)
  deriving (Enum, Num, Real, Integral) via Integer

instance GHC.KnownNat n => Bounded (T n) where
  minBound = 1
  maxBound = T . fromIntegral $ GHC.natVal @n Proxy

instance GHC.KnownNat n => Show (T n) where
  show (T n) = show k ++ " >= " ++ show n where T k = maxBound :: T n



type Power a b = a GHC.^ b

type Sum a b = a GHC.+ b

type Product a b = a GHC.* b

type Difference a b = a GHC.- b

type Quotient a b = GHC.Div a b

type Half a = GHC.Log2 a



infix 4 /~, ~~

type a /~ b = EqualType a b ~ 'False

type a ~~ b = EqualType a b ~ 'True

type family EqualType a b :: Bool
  where
    EqualType a a = 'True
    EqualType a b = 'False



type CompareNat a b = GHC.CmpNat a b

type Order = GHC.Ordering


infix 4 /=, <=, >=, ==, <, >

type a /= b = EqualOrder (CompareNat a b) 'EQ ~ 'False

type a <= b = EqualOrder (CompareNat a b) 'GT ~ 'False

type a >= b = EqualOrder (CompareNat a b) 'LT ~ 'False

type a == b = EqualOrder (CompareNat a b) 'EQ ~ 'True

type a < b = EqualOrder (CompareNat a b) 'LT ~ 'True

type a > b = EqualOrder (CompareNat a b) 'GT ~ 'True

type family EqualOrder (a :: Order) (b :: Order) :: Bool
  where
    EqualOrder a a = 'True
    EqualOrder a b = 'False

