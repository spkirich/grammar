-- | This module provides general types and functions.

module Grammar
  ( -- * General types

    Grammar    (..)
  , Production (..)

    -- * Normalization

  , eliminateUselessNonterminals

    -- * Regularity check

    -- * Regular approximation

  ) where

import Grammar.Definition
import Grammar.Normalization
