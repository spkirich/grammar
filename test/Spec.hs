{-# LANGUAGE TemplateHaskell #-}

import Data.Either

import System.Exit

import Test.QuickCheck

import Grammar
import Production

instance (Arbitrary t, Arbitrary n) => Arbitrary (Production t n) where

  -- | An arbitrary production
  arbitrary = Production <$> arbitrary <*> arbitrary

instance (Arbitrary t, Ord t, Arbitrary n, Ord n) => Arbitrary (Grammar t n) where

  -- | An arbitrary grammar
  arbitrary = Grammar <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | Test elimination of non-generating nonterminals efficiency
prop_eliminateNonGeneratingNonterminalsEffective :: Grammar Char Int -> Bool
prop_eliminateNonGeneratingNonterminalsEffective g
  = nonterminals (eliminateNonGeneratingNonterminals g) == generatingNonterminals g

return []
runTests = $quickCheckAll

main :: IO ()
main = runTests >>= \success -> if success
  then exitSuccess
  else exitFailure
