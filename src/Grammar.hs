-- | A definition of a grammar

module Grammar
  ( -- * Grammar
    Grammar (..)

  ) where

import           Data.Set   (Set)
import qualified Data.Set as Set

import Production

-- | A grammar
data Grammar t n = Grammar

  { grammarTerminals :: Set t
    -- ^ A set of terminals

  , grammarNonterminals :: Set n
    -- ^ A set of nonterminals

  , grammarProductions :: Set (Production t n)
    -- ^ A set of productions

  , grammarStartNonterminal :: n
    -- ^ The start nonterminal

  } deriving (Show)
