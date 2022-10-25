-- | Production definition

module Production
  ( Production (..)

    -- * Right hand side

  , rhsTerminals
  , rhsNonterminals

  ) where

import Data.Either (partitionEithers)

import           Data.Set   (Set)
import qualified Data.Set as Set

-- | A production of a grammar
data Production t n = Production

  { lhs :: n
    -- ^ A nonterminal on the left hand side

  , rhs :: [Either t n]
    -- ^ A sequence of symbols on the right hand side

  } deriving (Eq, Ord, Show)

-- | A set of symbols on the right hand side of a production
rhsSymbols :: (Ord t, Ord n) => Production t n -> (Set t, Set n)
rhsSymbols (Production _ r) = (Set.fromList ts, Set.fromList ns) where
  (ts, ns) = partitionEithers r

-- | A set of terminals on the right hand side of a production
rhsTerminals :: (Ord t, Ord n) => Production t n -> Set t
rhsTerminals = fst . rhsSymbols

-- | A set of nonterminals on the right hand side of a production
rhsNonterminals :: (Ord t, Ord n) => Production t n -> Set n
rhsNonterminals = snd . rhsSymbols
