{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

import GHC.TypeNats.Compat

import Math.NumberTheory.Moduli.Class             (KnownNat, Mod)
import Math.NumberTheory.Moduli.PrimitiveRoot     (PrimitiveRoot)

data OddPow (p :: Nat) (k :: Nat) = OddPow (PrimitiveRoot (p^k)) (Mod (p^(k-1)*(p-1)))
data TwoPow (k :: Nat) = TwoPow (Mod 2) (Mod (2^(k - 2)))

-- deriving instance (KnownNat (p^k), KnownNat (p^(k-1) * (p-1))) => Show (OddPow p k)
-- deriving instance (KnownNat (2^(k-2))) => Show (TwoPow k)

data DirichletCharacter (n :: Nat) where
  TrivialFactor :: DirichletCharacter 1
  TwoTerm :: DirichletCharacter 2
  TwoPower :: (2 <= k) => TwoPow k -> DirichletCharacter (2^k)
  OddPower :: OddPow p k -> DirichletCharacter n -> DirichletCharacter ((p^k) * n)

-- combine :: DirichletCharacter n -> DirichletCharacter n -> DirichletCharacter n
-- combine TrivialFactor n = n
-- combine TwoTerm TwoTerm = TwoTerm
-- combine (TwoPower (TwoPow a1 b1)) (TwoPower (TwoPow a2 b2)) = TwoPower (TwoPow (a1+a2) (b1+b2))
-- combine (OddPower (OddPow g a1) t1) (OddPower (OddPow _ a2) t2) = (OddPower (OddPow g (a1 + a2)) (combine t1 t2))

-- combine' :: DirichletCharacter (2^k) -> DirichletCharacter (2^k) -> DirichletCharacter (2^k)
-- combine' (Odd) = _

-- test :: DirichletHelper (a ': b) -> DirichletHelper (a ': b) -> DirichletHelper (a ': b)
-- test a b = case (a,b) of
--              (TrivialFactor, TrivialFactor) -> TrivialFactor
--              (TwoTerm, TwoTerm) -> TwoTerm
--              (TwoPower t, TwoPower u) -> no

no :: a
no = undefined
