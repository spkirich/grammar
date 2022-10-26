-- | This module provides some grammar normalization functions.

module Grammar.Normalization
  ( -- * Useless nonterminals

    generatingNonterminals
  , eliminateNonGeneratingNonterminals

    -- * Long productions

    -- * Epsilon-productions

    -- * Unit productions

    -- * Chomsky normal form

    -- * Left recursion

    -- * Greibach weak normal form

    -- * Kuroda normal form

  ) where

import Data.Set (Set)
import qualified Data.Set as Set

import Grammar.Definition

-- | A set of all generating nonterminals of the grammar.
generatingNonterminals :: (Ord t, Ord n) => Grammar t n -> Set n
generatingNonterminals g = fst $ until (uncurry (==)) (\(_, ns) -> (ns, step ns)) (Set.empty, basis) where
  step ns = Set.union ns . Set.map lhs . Set.filter (all (`Set.member` ns) . rhsNonterminals) $ productions g
  basis = Set.map lhs . Set.filter (null . rhsNonterminals) $ productions g

-- | Eliminate all non-generating nonterminals of the grammar.
eliminateNonGeneratingNonterminals :: (Ord t, Ord n) => Grammar t n -> Grammar t n
eliminateNonGeneratingNonterminals g = g { nonterminals = gns, productions = gps } where
  gps = Set.filter (\p -> Set.member (lhs p) gns && all (`Set.member` gns) (rhsNonterminals p)) $ productions g
  gns = generatingNonterminals g
