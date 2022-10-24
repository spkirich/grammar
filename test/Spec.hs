{-# LANGUAGE TemplateHaskell #-}

import Data.Either

import System.Exit

import Test.QuickCheck

import Production

instance (Arbitrary t, Arbitrary n) => Arbitrary (Production t n) where

  -- | Arbitrary production
  arbitrary = Production <$> arbitrary <*> arbitrary

-- | Test production right hand side partitionability
prop_productionRhsPartitionable :: Production Char Int -> Bool
prop_productionRhsPartitionable p = testTs && testNs
  where
    testTs = if all isRight $ productionRhs p
      then       null ts
      else not $ null ts

    testNs = if all isLeft  $ productionRhs p
      then       null ns
      else not $ null ns

    ts = productionRhsTerminals    p
    ns = productionRhsNonterminals p

return []
runTests = $quickCheckAll

main :: IO ()
main = runTests >>= \success -> if success
  then exitSuccess
  else exitFailure
