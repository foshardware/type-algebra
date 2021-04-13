{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- common quotient types
module Type.Algebra.Quotient where

import Control.Lens

import Type.Algebra.Arithmetic
import Type.Algebra.Natural


type Distinct k v = v / k ^ T 1

distinct :: (v -> k) -> v -> Distinct k v
distinct f v = quotient v (E . const . f)

getDistinct :: Distinct k v -> v
getDistinct = enumerator


-- set theorists would prefer "Z / 2Z"
type Mod (n :: Nat) z = z / T n ^ z

-- why is Natural 2 not inferred?
parity :: Natural 2 => z -> Mod 2 z
parity = view moderator

moderator :: (Natural m, Natural n) => Iso a b (Mod m a) (Mod n b)
moderator = iso (Q (E (const maxBound))) enumerator

modulo :: (Natural n, Integral z) => Mod n z -> z
modulo (Q (E f) z) = z `mod` fromIntegral (f z)

