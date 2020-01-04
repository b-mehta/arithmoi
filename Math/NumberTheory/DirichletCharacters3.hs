{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneDeriving, UndecidableInstances #-}

import GHC.TypeNats.Compat
import Data.Singletons.TypeLits hiding (Mod)
import qualified Data.Singletons.TypeLits as TL
import Data.Constraint.Nat hiding (Mod)
import Data.Constraint

import Data.Ratio
import Data.Semigroup hiding (Product)
import Math.NumberTheory.Moduli.Class
import Math.NumberTheory.Moduli.PrimitiveRoot     (PrimitiveRoot)
import Math.NumberTheory.Moduli.DiscreteLogarithm

newtype RootOfUnity =
  RootOfUnity { -- | Every root of unity can be expressed as \(e^{2 \pi i q}\) for some
                -- rational \(q\) satisfying \(0 \leq q < 1\), this function extracts it.
                fromRootOfUnity :: Rational }
  deriving (Eq)

instance Show RootOfUnity where
  show (RootOfUnity q)
    | n == 0    = "e^0"
    | d == 1    = "e^(πi)"
    | n == 1    = "e^(πi/" ++ show d ++ ")"
    | otherwise = "e^(" ++ show n ++ "πi/" ++ show d ++ ")"
    where n = numerator (2*q)
          d = denominator (2*q)

-- | Given a rational \(q\), produce the root of unity \(e^{2 \pi i q}\).
toRootOfUnity :: Rational -> RootOfUnity
toRootOfUnity q = RootOfUnity ((n `rem` d) % d)
  where n = numerator q
        d = denominator q
        -- effectively q `mod` 1
  -- This smart constructor ensures that the rational is always in the range 0 <= q < 1.

-- | This Semigroup is in fact a group, so @stimes@ can be called with a negative first argument.
instance Semigroup RootOfUnity where
  (RootOfUnity q1) <> (RootOfUnity q2) = toRootOfUnity (q1 + q2)
  stimes k (RootOfUnity q) = toRootOfUnity (q * fromIntegral k)

instance Monoid RootOfUnity where
  mappend = (<>)
  mempty = RootOfUnity 0

data OddPow (p :: Nat) (k :: Nat) = OddPow (PrimitiveRoot (p^k)) (Mod (p^(k-1)*(p-1)))
data TwoPow (k :: Nat) = TwoPow (Mod 2) (Mod (2^(k - 2)))

deriving instance (KnownNat (p^k), KnownNat (p^(k-1) * (p-1))) => Show (OddPow p k)
deriving instance (KnownNat (2^(k-2))) => Show (TwoPow k)

data DirichletCharacter (n :: Nat) where
  Generated :: DirichletList xs -> DirichletCharacter (Product xs)

type family Product (n :: [*]) :: Nat where
  Product '[] = 1
  Product ((SNat p, SNat k) ': xs) = p^k * Product xs

data DirichletList (n :: [*]) where
  TrivialFactor :: DirichletList '[]
  TwoTerm :: DirichletList '[(SNat 2, SNat 1)]
  TwoPower :: (2 <= k, KnownNat (2^(k-2))) => TwoPow k -> DirichletList '[(SNat 2, SNat k)]
  OddPower' :: (KnownNat (p^(k-1)*(p-1)), KnownNat (p^k), TL.Mod p 2 ~ 1, 1 <= k) => OddPow p k -> DirichletList '[(SNat p, SNat k)]
  OddPower :: ( KnownNat (Product ((SNat q, a) ': b))
              , KnownNat (p^(k-1)*(p-1))
              , KnownNat (p^k)
              , 1 <= k
              , q <= p
              , TL.Mod p 2 ~ 1)
           => OddPow p k
           -> DirichletList ((SNat q, a) ': b)
           -> DirichletList ((SNat p, SNat k) ': (SNat q, a) ': b)

multiply :: DirichletList n -> DirichletList n -> DirichletList n
multiply a b = case (a,b) of
             (TrivialFactor, TrivialFactor) -> TrivialFactor
             (TwoTerm, TwoTerm) -> TwoTerm
             (TwoPower (TwoPow a1 b1), TwoPower (TwoPow a2 b2)) ->
               TwoPower (TwoPow (a1+a2) (b1+b2))
             (OddPower' (OddPow g a1), OddPower' (OddPow _ a2)) ->
               OddPower' (OddPow g (a1+a2))
             (OddPower (OddPow g a1) xs, OddPower (OddPow _ a2) ys) ->
               OddPower (OddPow g (a1+a2)) (multiply xs ys)

apply :: DirichletList n -> MultMod (Product n) -> RootOfUnity
apply TrivialFactor _ = mempty
apply TwoTerm _ = mempty
apply (TwoPower (TwoPow _a _b)) _x = undefined
apply (OddPower' o) x = runFactor o x
apply (OddPower o t) x = runFactor o a <> apply t b
  where (a,b) = reduceMult x

runFactor :: (KnownNat (p^(k-1)*(p-1)), KnownNat (p^k)) => OddPow p k -> MultMod (p^k) -> RootOfUnity
runFactor (OddPow g a) x = modToRoot $ a * (fromIntegral $ discreteLogarithm g x)

modToRoot :: KnownNat n => Mod n -> RootOfUnity
modToRoot n = toRootOfUnity (getVal n % getMod n)
