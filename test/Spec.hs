{-# LANGUAGE TemplateHaskell #-}

import Data.Either

import System.Exit

import Test.QuickCheck

import Grammar
import Production

instance (Arbitrary t, Arbitrary n) => Arbitrary (Production t n) where

  -- | An arbitrary production
  arbitrary = Production <$> arbitrary <*> arbitrary

return []
runTests = $quickCheckAll

main :: IO ()
main = runTests >>= \success -> if success
  then exitSuccess
  else exitFailure
