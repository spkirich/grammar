-- | This module provides some grammar normalization functions.

module Grammar.Normalization
  ( -- * Useless nonterminals

    eliminateNonterminals

  ,   reachableNonterminals
  , unreachableNonterminals

  ,    generatingNonterminals
  , nonGeneratingNonterminals

  , eliminateUselessNonterminals

    -- * Long productions

  , longProductions
  , eliminateLongProductions

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

-- | Eliminate a given set of the grammar nonterminals.
eliminateNonterminals :: (Ord t, Ord n) => Set n -> Grammar t n -> Grammar t n
eliminateNonterminals ns g = g { nonterminals = Set.difference (nonterminals g) ns, productions = ps } where
  ps = Set.filter (\p -> not $ Set.member (lhs p) ns && all (`Set.member` ns) (rhsNonterminals p)) $ productions g

-- | A set of all reachable grammar nonterminals.
reachableNonterminals :: (Ord t, Ord n) => Grammar t n -> Set n
reachableNonterminals g = fst $ until (uncurry (==)) (\(_, ns) -> (ns, step ns)) (Set.empty, basis) where
  step ns = Set.union ns . Set.unions . Set.map rhsNonterminals . Set.filter ((`Set.member` ns) . lhs) $ productions g
  basis = Set.singleton $ startNonterminal g

-- | A set of all unreachable grammar nonterminals.
unreachableNonterminals :: (Ord t, Ord n) => Grammar t n -> Set n
unreachableNonterminals g = Set.difference (nonterminals g) $ reachableNonterminals g

-- | A set of all generating grammar nonterminals.
generatingNonterminals :: (Ord t, Ord n) => Grammar t n -> Set n
generatingNonterminals g = fst $ until (uncurry (==)) (\(_, ns) -> (ns, step ns)) (Set.empty, basis) where
  step ns = Set.union ns . Set.map lhs . Set.filter (all (`Set.member` ns) . rhsNonterminals) $ productions g
  basis = Set.map lhs . Set.filter (null . rhsNonterminals) $ productions g

-- | A set of all non-generating grammar nonterminals.
nonGeneratingNonterminals :: (Ord t, Ord n) => Grammar t n -> Set n
nonGeneratingNonterminals g = Set.difference (nonterminals g) $ generatingNonterminals g

-- | Eliminate all useless nonterminals from the grammar.
eliminateUselessNonterminals :: (Ord t, Ord n) => Grammar t n -> Grammar t n
eliminateUselessNonterminals g = eliminateNonterminals (nonGeneratingNonterminals h) h where
  h = eliminateNonterminals (unreachableNonterminals g) g

-- | A set of all long grammar productions.
longProductions :: Grammar t n -> Set (Production t n)
longProductions = Set.filter ((> 2) . length . rhs) . productions

-- | Eleminate all long productions from the grammar.
eliminateLongProductions :: (Ord t, Enum n, Ord n) => Grammar t n -> Grammar t n
eliminateLongProductions g = foldr eliminate g $ longProductions g where
  eliminate p h = let ns = take (length (rhs p) - 2) $ freeNonterminals h in h
    { nonterminals = Set.union (nonterminals h) $ Set.fromList ns
    , productions  = Set.delete p . Set.union (productions h) . Set.fromList $
        zipWith3 (\l q r -> Production l [q, r])
          (lhs p : ns) (init $ rhs p) $ map Right ns ++ [last $ rhs p]
    }
