-- | Grammar definition

module Grammar
  ( Grammar (..)

  ) where

import           Data.Set   (Set)
import qualified Data.Set as Set

import Production

-- | A grammar
data Grammar t n = Grammar

  { terminals :: Set t
    -- ^ A set of terminals

  , nonterminals :: Set n
    -- ^ A set of nonterminals

  , productions :: Set (Production t n)
    -- ^ A set of productions

  , startNonterminal :: n
    -- ^ The start nonterminal

  } deriving (Show)
