{-# LANGUAGE ScopedTypeVariables #-}
import Data.Maybe

import Math.NumberTheory.Moduli
import Math.NumberTheory.Prefactored
import Math.NumberTheory.Primes
import GHC.Natural
import Control.Exception
import Control.Monad

allOddPrimes :: [Integer]
allOddPrimes = tail primes

allPrimitiveRoots :: CyclicGroup Integer -> [Integer]
allPrimitiveRoots cg = filter (isPrimitiveRoot' cg) [2..(prefValue $ cyclicGroupToModulo cg)]

allCases :: [(Integer, Integer, Integer)]
allCases = do
  p <- allOddPrimes
  cg <- mapMaybe cyclicGroupFromModulo [p]
  a <- allPrimitiveRoots cg
  b <- [1..p-1]
  return (p,a,b)

run :: [Natural]
run = [discreteLogarithmPrimePollard p a b | (p,a,b) <- take 10 allCases] ++ error "error happened"

main :: IO ()
main = do
  putStrLn "testing first 1000 odd primes"
  forM_ (take 1000 allOddPrimes) runPrime

runPrime :: Integer -> IO ()
runPrime p = do
  putStrLn $ "testing p=" ++ show p
  let Just cg = cyclicGroupFromModulo p -- always succeeds
  forM_ (allPrimitiveRoots cg) (runPrimRoot p)

runPrimRoot :: Integer -> Integer -> IO ()
runPrimRoot p a = do
  forM_ [1..p-1] (runDiscLog p a)

runDiscLog :: Integer -> Integer -> Integer -> IO ()
runDiscLog p a = void . evaluate . discreteLogarithmPrimePollard p a
