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

import           Data.Set (Set)
import qualified Data.Set as Set

-- | @CFG v t@ is the type of context-free grammars with variables of type @v@
-- and terminals of type @t@. Each such grammar contains the set of 'Rule's
-- and the start symbol. The list of terminals and non-terminals does not need
-- to be explicitly stated for our purposes.
data CFG v t = CFG { rules :: Set (Rule v t) -- ^ Set of rules in the CFG.
                   , start :: v              -- ^ The start symbol.
                   }
    deriving Show

-- | @Rule v t@ is a CFG rule with variables of type @v@ and terminals of type
-- @t@. Variables with more than one rule should have multiple @Rule@s, for
-- example, \(S \to AB \mid \epsilon\) should have rules @[S :-> [Left A, Left
-- B], S :-> []]@.
data Rule v t =
    -- | @S :-> s@ is the rule generating the string given by the list @s@ from
    -- the variable @S@.
    v :-> Production v t
    deriving (Show, Eq, Ord)

-- | The type of productions in a 'Rule'. The string is a list of 'Either's,
-- with 'Left's representing variables and 'Right's representing terminals.
type Production v t = [Either v t]

-- | Acts like \(\to\) in normal CFGs for use with '.|' and '.>'. To write
-- \(S \to AB \mid \epsilon\), for example, write
--
-- > S .-> [Left A, Left B] .| [] .>
(.->) :: (Ord v, Ord t) => v -> Set (Production v t) -> Set (Rule v t)
(.->) s = Set.map (s :->)

-- | Acts like \(\mid\) in normal CFGs for use with '.->' and '.>'.
(.|) :: (Ord v, Ord t)
     => Production v t -> Set (Production v t) -> Set (Production v t)
(.|) = Set.insert

-- | Terminates a list of alternatives using '.->' and '.|'. Necessary so that
-- such expressions are well typed.
(.>) :: (Ord v, Ord t) => Production v t -> Set (Production v t)
(.>) = Set.singleton
