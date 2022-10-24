-- | A definition of a production

module Production
  ( -- * Production
    Production (..)

    -- * Right hand side
  , productionRhsTerminals
  , productionRhsNonterminals

  ) where

import Data.Either (partitionEithers)

import           Data.Set   (Set)
import qualified Data.Set as Set

-- | A production of a grammar
data Production t n = Production

  { productionLhs :: n
    -- ^ A nonterminal on the left hand side

  , productionRhs :: [Either t n]
    -- ^ A sequence of symbols on the right hand side

  } deriving (Eq, Ord, Show)

-- | A set of symbols on the right hand side of a production
productionRhsSymbols :: (Ord t, Ord n) => Production t n -> (Set t, Set n)
productionRhsSymbols (Production _ r) = (Set.fromList ts, Set.fromList ns) where
  (ts, ns) = partitionEithers r

-- | A set of terminals on the right hand side of a production
productionRhsTerminals :: (Ord t, Ord n) => Production t n -> Set t
productionRhsTerminals = fst . productionRhsSymbols

-- | A set of nonterminals on the right hand side of a production
productionRhsNonterminals :: (Ord t, Ord n) => Production t n -> Set n
productionRhsNonterminals = snd . productionRhsSymbols
