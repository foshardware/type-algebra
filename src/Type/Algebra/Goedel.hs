{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}

module Type.Algebra.Goedel where

import Control.Newtype
import Control.Monad.Reader (ReaderT(..))
import Data.Bits
import Data.Functor.Identity
import Data.Hashable
import Data.Typeable
import Data.Word

import Type.Algebra.Natural



jump :: Int
jump = fromIntegral @Word64 0x819ac082a96b3ab5


type Number = Word


newtype HigherKinded = HigherKinded Number

instance Newtype HigherKinded Number

instance Semigroup HigherKinded where
    HigherKinded a <> HigherKinded b = HigherKinded (xor a b)

instance Monoid HigherKinded where
    mempty = HigherKinded 0


-- material implication in terms of goedel numbers but the arrow is not flipped
(==>) :: Number -> Number -> Number
p ==> q = negate p `xor` q -- bitwise or converges toward maxBound


recover :: Number -> Number
recover 0
  = error "cannot recover from void"
recover w
  | 0 <- w .&. pred w
  , m <- hashWithSalt jump w
  = fromIntegral m
recover w = w


goedelConstant :: Number -> Bool
goedelConstant = lt where
  lt 0 = True
  lt 1 = True
  lt 2 = True
  lt _ = False


typeGoedel :: Typeable a => Proxy a -> Number
typeGoedel
  = recover
  . fromIntegral
  . hash
  . key
  . typeRepTyCon
  . typeRep
  where
    key v = [tyConPackage v, tyConModule v, tyConName v]



class Typeable a => Goedel a where

  goedel :: a -> Number
  goedel _ = goedel (Proxy @a)


instance Goedel a => Goedel (Proxy a) where
  goedel = typeGoedel


instance Goedel () where
  goedel = const 1

instance Goedel Bool where
  goedel = const 2


instance Natural n => Goedel (T n) where
  goedel _ = fromIntegral (maxBound :: T n)


instance (Goedel a, Goedel b) => Goedel (a, b) where
  goedel (a, b) = goedel a * goedel b

instance (Goedel a, Goedel b, Goedel c) => Goedel (a, b, c) where
  goedel (a, b, c) = goedel a * goedel (b, c)

instance (Goedel a, Goedel b, Goedel c, Goedel d) => Goedel (a, b, c, d) where
  goedel (a, b, c, d) = goedel a * goedel (b, c, d)

instance (Goedel a, Goedel b, Goedel c, Goedel d, Goedel e) => Goedel (a, b, c, d, e) where
  goedel (a, b, c, d, e) = goedel a * goedel (b, c, d, e)


instance (Goedel a, Goedel b) => Goedel (a -> b) where
  goedel _ = goedel (Proxy @b) ^ goedel (Proxy @a)


instance (Goedel a, Goedel b) => Goedel (Either a b) where
  goedel _ = goedel (Proxy @a) + goedel (Proxy @b)

instance Goedel a => Goedel (Maybe a) where
  goedel _ = goedel (Proxy @a) + goedel ()


instance Goedel a => Goedel (Identity a) where
  goedel = goedel . runIdentity


instance (Typeable m, Typeable a, Goedel (m a), Goedel r) => Goedel (ReaderT r m a) where
  goedel = goedel . runReaderT



instance Typeable a => Goedel [a] where
  goedel = kind2

instance Typeable a => Goedel (IO a) where
  goedel = kind2



kind2
  :: forall f a. Typeable f
  => Typeable a
  => f a
  -> Number
kind2 _
  = ala HigherKinded foldMap
  [ typeGoedel (Proxy :: Proxy f) 
  , typeGoedel (Proxy :: Proxy a)
  ]


kind3
  :: forall f a b. Typeable f
  => Typeable a
  => Typeable b
  => f a b
  -> Number
kind3 _
  = ala HigherKinded foldMap
  [ typeGoedel (Proxy :: Proxy f)
  , typeGoedel (Proxy :: Proxy a)
  , typeGoedel (Proxy :: Proxy b)
  ]


kind4
  :: forall f a b c. Typeable f
  => Typeable a
  => Typeable b
  => Typeable c
  => f a b c
  -> Number
kind4 _
  = ala HigherKinded foldMap
  [ typeGoedel (Proxy :: Proxy f)
  , typeGoedel (Proxy :: Proxy a)
  , typeGoedel (Proxy :: Proxy b)
  , typeGoedel (Proxy :: Proxy c)
  ]

