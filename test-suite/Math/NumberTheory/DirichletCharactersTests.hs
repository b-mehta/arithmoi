-- |
-- Module:       Math.NumberTheory.Moduli.DiscreteLogarithm
-- Copyright:    (c) 2018 Bhavik Mehta
-- License:      MIT
-- Maintainer:   Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:    Provisional
-- Portability:  Non-portable
--
-- Tests for Math.NumberTheory.DirichletCharacters
--

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}

module Math.NumberTheory.DirichletCharactersTests where

import Test.Tasty

import Data.Proxy
import Data.Ratio
import Numeric.Natural
import Data.Semigroup
import Data.Complex
import Data.List (nub, genericLength, genericReplicate, isSubsequenceOf)
import Data.Maybe (mapMaybe, isJust)

import GHC.TypeNats.Compat (SomeNat(..), someNatVal, KnownNat, natVal)

import Math.NumberTheory.ArithmeticFunctions (totient)
import Math.NumberTheory.DirichletCharacters
import Math.NumberTheory.Moduli.Class (SomeMod(..), modulo)
import Math.NumberTheory.TestUtils (testSmallAndQuick, Positive(..))

rootOfUnityTest :: Integer -> Positive Integer -> Bool
rootOfUnityTest n (Positive d) = toComplex ((d `div` gcd n d) `stimes` toRootOfUnity (n % d)) == (1 :: Complex Double)

-- | This tests property 6 from https://en.wikipedia.org/wiki/Dirichlet_character#Axiomatic_definition
dirCharOrder :: Positive Natural -> Natural -> Bool
dirCharOrder (Positive n) i = case someNatVal n of
                                SomeNat (Proxy :: Proxy n) -> (totient n) `stimes` chi == principalChar
                                  where chi = fromIndex (i `mod` (totient n)) :: DirichletCharacter n

-- | Tests wikipedia's property 3 (note 1,2,5 are essentially enforced by the type system).
testMultiplicative :: KnownNat n => DirichletCharacter n -> Natural -> Natural -> Bool
testMultiplicative chi a b = chiAB == chiAchiB
  where chi' = generalEval chi
        a' = fromIntegral a
        b' = fromIntegral b
        chiAB = chi' (a'*b')
        chiAchiB = (<>) <$> chi' a' <*> chi' b'

-- | Test property 4 from wikipedia
testAtOne :: KnownNat n => DirichletCharacter n -> Bool
testAtOne chi = evaluate chi mempty == mempty

dirCharProperty :: (forall n. KnownNat n => DirichletCharacter n -> a) -> Positive Natural -> Natural -> a
dirCharProperty test (Positive n) i = case someNatVal n of
                                        SomeNat (Proxy :: Proxy n) -> test chi
                                          where chi = fromIndex (i `mod` (totient n)) :: DirichletCharacter n

-- | There should be phi(n) characters
countCharacters :: Positive Natural -> Bool
countCharacters (Positive n) = case someNatVal n of
                                 SomeNat (Proxy :: Proxy n) ->
                                   genericLength (nub [minBound :: DirichletCharacter n .. maxBound]) == totient n

-- | The principal character should be 1 at all phi(n) places
principalCase :: Positive Natural -> Bool
principalCase (Positive n) = case someNatVal n of
                             SomeNat (Proxy :: Proxy n) -> mapMaybe (generalEval chi) [minBound..maxBound] == genericReplicate (totient n) mempty
                               where chi = principalChar :: DirichletCharacter n

-- | Test the orthogonality relations https://en.wikipedia.org/wiki/Dirichlet_character#Character_orthogonality
orthogonality1 :: forall n. KnownNat n => DirichletCharacter n -> Bool
orthogonality1 chi = magnitude (total - correct) < 1e-14
  where n = natVal (Proxy :: Proxy n)
        total = sum [toFunction chi a | a <- [0..n-1]]
        correct = if isPrincipal chi
                     then fromIntegral $ totient n
                     else 0

orthogonality2 :: Positive Natural -> Integer -> Bool
orthogonality2 (Positive n) a = case a `modulo` n of
                                  SomeMod a' -> magnitude (total - correct) < 1e-13
                                    where total = sum [maybe 0 toComplex (generalEval chi a') | chi <- [minBound .. maxBound]]
                                          correct = if a' == 1
                                                       then fromIntegral $ totient n
                                                       else 0
                                  InfMod {} -> False

realityCheck :: forall n. KnownNat n => DirichletCharacter n -> Bool
realityCheck chi = isJust (isRealCharacter chi) == isReal'
  where isReal' = nub (mapMaybe (generalEval chi) [minBound..maxBound]) `isSubsequenceOf` [mempty, toRootOfUnity (1 % 2)]

testSuite :: TestTree
testSuite = testGroup "DirichletCharacters"
  [ testSmallAndQuick "RootOfUnity contains roots of unity" rootOfUnityTest
  , testSmallAndQuick "Dirichlet characters divide the right order" dirCharOrder
  , testSmallAndQuick "Dirichlet characters are multiplicative" (dirCharProperty testMultiplicative)
  , testSmallAndQuick "Dirichlet characters are 1 at 1" (dirCharProperty testAtOne)
  , testSmallAndQuick "Right number of Dirichlet characters" countCharacters
  , testSmallAndQuick "Principal character behaves as expected" principalCase
  , testSmallAndQuick "Orthogonality relation 1" (dirCharProperty orthogonality1)
  , testSmallAndQuick "Orthogonality relation 2" orthogonality2
  , testSmallAndQuick "Real character checking is valid" (dirCharProperty realityCheck)
  ]
