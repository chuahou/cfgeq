-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

-----------------------------------------------------------------------------
-- |
-- Module      : CFGEq.CFG
-- Copyright   : Chua Hou 2021
-- License     : MIT
--
-- Maintainer  : Chua Hou <human+github@chuahou.dev>
-- Stability   : experimental
-- Portability : non-portable
--
-- The DSL used to describe CFGs.
-----------------------------------------------------------------------------

module CFGEq.CFG where

import           Data.List          (intercalate)
import           Data.List.NonEmpty (NonEmpty (..))

-- | @CFG v t@ is the type of context-free grammars with variables of type @v@
-- and terminals of type @t@. Each such grammar is a 4-tuple of the list of
-- variables, list of terminals, list of 'Rule's and the start symbol.
type CFG v t = ([v], [t], [Rule v t], v)

-- | @Rule v t@ is a CFG rule with variables of type @v@ and terminals of type
-- @t@. Variables with more than one rule should have multiple @Rule@s, for
-- example, \(S \to AB \mid \epsilon\) should have rules @[S :-> [Left A, Left
-- B], S :-> []]@.
data Rule v t =
    -- | @S :-> s@ is the rule generating the string given by the list @s@ from
    -- the variable @S@. The string is a list of 'Either's, with 'Left's
    -- representing variables and 'Right's representing terminals.
    v :-> [Either v t]

instance (Show v, Show t) => Show (Rule v t) where
    show (var :-> [])    = show var <> " -> Epsilon"
    show (var :-> terms) = show var <> " -> " <> sterms
        where
            -- Each element on the right is either displayed as a string if each
            -- corresponds to a single character, or a comma-separated list
            -- otherwise.
            sterms = if all ((== 1) . length) sterms'
                        then concat sterms'
                        else intercalate ", " sterms'
            sterms' = map (either show show) terms

-- | Acts similarly to \(\mid\) in normal CFGs, adding a new rule to a
-- 'NonEmpty' list of existing rules. For example, \(S \to AB \mid c\) would be
-- written @S .-> [Left A, Left B] .| [Right \'c']@.
(.|) :: NonEmpty (Rule v t) -> [Either v t] -> NonEmpty (Rule v t)
(s :-> w :| rs) .| w' = s :-> w :| s :-> w' : rs

-- | Creates a single rule as a 'NonEmpty' list, allowing for use with '.|' to
-- write multiple rules with the same terminal.
(.->) :: v -> [Either v t] -> NonEmpty (Rule v t)
s .-> w = s :-> w :| []
