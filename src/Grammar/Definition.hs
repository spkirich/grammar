-- | This module defines a grammar.

module Grammar.Definition
  ( -- * Grammar

    Grammar (..)

    -- * Production

  , Production (..)

  , rhsTerminals
  , rhsNonterminals

  ) where

import Data.Either

import Data.Set (Set)
import qualified Data.Set as Set

-- | A grammar is exactly:
data Grammar t n = Grammar

  { terminals :: Set t
    -- ^ a set of terminals;

  , nonterminals :: Set n
    -- ^ a set of nonterminals;

  , productions :: Set (Production t n)
    -- ^ a set of productions;

  , startNonterminal :: n
    -- ^ the start nonterminal.

  } deriving (Show)

-- | A production is exactly:
data Production t n = Production

  { lhs :: n
    -- ^ a nonterminal on the left-hand side;

  , rhs :: [Either t n]
    -- ^ a sequence of symbols on the right-hand side.

  } deriving (Eq, Ord, Show)

-- | A set of symbols on the right-hand side of a production.
rhsSymbols :: (Ord t, Ord n) => Production t n -> (Set t, Set n)
rhsSymbols (Production _ r) = (Set.fromList ts, Set.fromList ns) where
  (ts, ns) = partitionEithers r

-- | A set of terminals on the right-hand side of a production.
rhsTerminals :: (Ord t, Ord n) => Production t n -> Set t
rhsTerminals = fst . rhsSymbols

-- | A set of nonterminals on the right-hand side of a production.
rhsNonterminals :: (Ord t, Ord n) => Production t n -> Set n
rhsNonterminals = snd . rhsSymbols
