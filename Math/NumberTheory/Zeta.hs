-- |
-- Module:      Math.NumberTheory.Zeta
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Riemann zeta-function.

{-# LANGUAGE ScopedTypeVariables, ParallelListComp #-}

module Math.NumberTheory.Zeta
  ( zetas
  , zetasEven
  , approximateValue
  , chudnovsky
  ) where

import Data.ExactPi
import Data.Ratio

import Math.NumberTheory.Recurrencies (bernoulli, factorial)

-- | Infinite sequence of exact values of Riemann zeta-function at even arguments, starting with @ζ(0)@.
-- Note that due to numerical errors convertation to 'Double' may return values below 1:
--
-- >>> approximateValue (zetasEven !! 25) :: Double
-- 0.9999999999999996
--
-- Use your favorite type for long-precision arithmetic. For instance, 'Data.Number.Fixed.Fixed' works fine:
--
-- >>> import Data.Number.Fixed
-- >>> approximateValue (zetasEven !! 25) :: Fixed Prec50
-- 1.00000000000000088817842111574532859293035196051773
--
zetasEven :: [ExactPi]
zetasEven = zipWith Exact [0, 2 ..] $ zipWith (*) (skipOdds bernoulli) cs
  where
    cs = (- 1 % 2) : zipWith (\i f -> i * (-4) / fromInteger (2 * f * (2 * f - 1))) cs [1..]

skipOdds :: [a] -> [a]
skipOdds (x : _ : xs) = x : skipOdds xs
skipOdds xs = xs

zetasEven' :: Floating a => [a]
zetasEven' = map approximateValue zetasEven

zetasOdd :: forall a. (Floating a, Ord a) => a -> [a]
zetasOdd eps = (1 / 0) : zets
  where
    zets :: [a] -- [zeta(3), zeta(5), zeta(7)...]
    zets = zipWith (*) zs (tail (iterate (* (- pi * pi)) 1))

    zs :: [a] -- [zeta(3) / (-pi^2), zeta(5) / pi^4, zeta(7) / (-pi^6)...]
    zs = zipWith (\w f -> negate (w / (1 + f))) ws fourth

    ys :: [a] -- [(1 - 1/4) * zeta(3) / (-pi^2), (1 - 1/4^2) * zeta(5) / pi^4...]
    ys = zipWith (*) zs fourth
    yss :: [[a]] -- [[], [ys !! 0], [ys !! 1, ys !! 0], [ys !! 2, ys !! 1, ys !! 0]...]
    yss = scanl (flip (:)) [] ys

    xs :: [a] -- first summand of RHS in (57) for m=[1..]
    xs = map (sum . zipWith (flip (/)) factorial2) yss

    ws :: [a] -- RHS in (57) for m=[1..]
    ws = zipWith (+) xs cs

    rs :: [a] -- [1, 1/2, 1/3, 1/4...]
    rs = map (\n -> recip (fromInteger n)) [1..]
    rss :: [[a]] -- [[1, 1/2, 1/3...], [1/2, 1/3, 1/4...], [1/3, 1/4...]]
    rss = iterate tail rs

    factorial2 :: [a] -- [2!, 4!, 6!..]
    factorial2 = map fromInteger $ tail $ skipOdds factorial

    fourth :: [a] -- [1 - 1/4, 1 - 1/4^2, 1 - 1/4^3...]
    fourth = tail $ map (1 -) $ iterate (/ 4) 1

    as :: [a] -- [zeta(0), zeta(2)/4, zeta(2*2)/4^2, zeta(2*3)/4^3...]
    as = zipWith (/) zetasEven' (iterate (* 4) 1)

    bs :: [a] -- map (+ log 2) [b(1), b(2), b(3)...],
              -- where b(m) = \sum_{n=0}^\infty zeta(2n) / 4^n / (n + m)
    bs = map ((+ log 2) . suminf eps . zipWith (*) as) rss

    cs :: [a] -- second summand of RHS in (57) for m = [1..]
    cs = zipWith (\b f -> b / f) bs factorial2

suminf :: (Floating a, Ord a) => a -> [a] -> a
suminf eps = sum . takeWhile ((>= eps / 111) . abs)

-- | Infinite sequence of approximate (up to given precision)
-- values of Riemann zeta-function at integer arguments, starting with @ζ(0)@.
-- Computations for odd arguments are performed in accordance to
-- <https://cr.yp.to/bib/2000/borwein.pdf Computational strategies for the Riemann zeta function>
-- by J. M. Borwein, D. M. Bradley, R. E. Crandall, formula (57).
--
-- >>> take 5 (zetas 1e-14) :: [Double]
-- > [-0.5,Infinity,1.6449340668482262,1.2020569031595942,1.0823232337111381]
--
-- Beware to force evaluation of @zetas !! 1@, if the type @a@ does not support infinite values
-- (for instance, 'Data.Number.Fixed.Fixed').
--
zetas :: (Floating a, Ord a) => a -> [a]
zetas eps = e : o : scanl1 f (intertwine es os)
  where
    e : es = zetasEven'
    o : os = zetasOdd eps

    intertwine (x : xs) (y : ys) = x : y : intertwine xs ys
    intertwine      xs       ys  = xs ++ ys

    -- Cap-and-floor to improve numerical stability:
    -- 0 < zeta(n + 1) - 1 < (zeta(n) - 1) / 2
    f x y = 1 `max` (y `min` (1 + (x - 1) / 2))

-- | Infinite sequence of rational approximations to pi^2, using Chudnovsky's algorithm.
-- The first approximation is within 10^-12 of the true value while the second has the same
-- Double value as the true value.
chudnovsky :: [Rational]
chudnovsky = [426880^2 * 10005 / s^2 | s <- partials]
  where lk = iterate (+545140134) 13591409
        xk = iterate (*(-262537412640768000)) 1
        kk = iterate (+12) 6
        mk = 1: [m * ((k^3 - 16*k) % (n+1)^3) | m <- mk | k <- kk | n <- [0..]]
        values = [m * l / x | m <- mk | l <- lk | x <- xk]
        partials = scanl1 (+) values
