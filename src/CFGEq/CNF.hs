-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

-----------------------------------------------------------------------------
-- |
-- Module      : CFGEq.CNF
-- Copyright   : Chua Hou 2021
-- License     : MIT
--
-- Maintainer  : Chua Hou <human+github@chuahou.dev>
-- Stability   : experimental
-- Portability : non-portable
--
-- Compilation of CFGs from 'CFGEq.CFG' to Chomsky normal form.
-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

module CFGEq.CNF where

import           Control.Monad.State (State, get, put, runState)
import           Data.Map            (Map)
import qualified Data.Map            as Map

import           CFGEq.CFG

data CNF v t = CNF { rulesCNF      :: Map v [Either (v, v) t]
                   , startCNF      :: v
                   , producesEmpty :: Bool
                   }
    deriving Show

compile :: forall v t. Ord v => (Int -> v) -> CFG v t -> CNF v t
compile mkFresh (_, _, rules, s) = CNF rules' newS False
    where
        -- START step
        -- Generates a rule with a new start symbol that produces the old start
        -- symbol @oldS@.
        start :: v -> State Int (v, Rule v t)
        start oldS = get >>= \gen -> put (succ gen) >> pure
            (let s' = mkFresh gen
              in (s', s' :-> [Left oldS]))

        -- BIN step
        -- Converts given rule to a list of equivalent rules each with <= 2
        -- variables on the right hand side.
        bin :: Rule v t -> State Int [Rule v t]
        bin (var :-> (a:b:c:cs)) = do
            gen <- get
            let f = mkFresh gen
            put . succ $ gen
            recurse <- bin (f :-> (b:c:cs))
            pure $ (var :-> [a, Left f]) : recurse
        bin r = pure [r]

        -- DEL step
        -- Eliminates nullable rules, giving the new list of rules, given the
        -- start symbol and list of already removed rules. TODO: Think of more
        -- efficient way to do this.
        del :: v -> [v] -> [Rule v t] -> [Rule v t]
        del s' removed rs =
            case filter (\(var :-> w) -> null w && var /= s') rs of
              []            -> rs
              (var :-> _):_ -> del s' (var:removed) . concatMap (go var) $ rs
            where
                -- Generate the list of corresponding new rules based on @var@
                -- being nullable. Error impurely if > 2 symbols on RHS since
                -- we've already run BIN. TODO: More fine-grained types, see
                -- 'toCNF'.
                go var (var' :-> w)
                    | var == var' && null w = []  -- Remove this rule.
                    | otherwise = let lvar = Left var in map (var' :->) $
                        case w of
                          [v1, v2] ->
                              if v1 == lvar && v2 == lvar
                                 -- Only add epsilon rule if not removed before.
                                 then (if var' `elem` (var:removed) then id else ([]:))
                                                      [ [lvar, lvar], [lvar] ]
                              else if v1 == lvar then [ [v1, v2], [v2] ]
                              else if v2 == lvar then [ [v1, v2], [v1] ]
                              else                    [ [v1, v2] ]
                          [v] ->
                              if v == lvar
                                 -- Only add epsilon rule if not removed before.
                                 then (if var' `elem` (var:removed) then id else ([]:))
                                    [ [lvar] ]
                                 else [ [v] ]
                          []  -> [[]]
                          _   -> error "del internal error"

        -- UNIT step
        -- Eliminates unit rules of form \(A \to B\).
        unit :: [Rule v t] -> [Rule v t]
        unit = undefined

        -- TERM step
        -- Converts given rule into one with no nonsolitary terminals.
        term :: Rule v t -> State Int [Rule v t]
        term = undefined

        -- Converts a CFG rule into a CNF rule. Errors impurely when the CFG
        -- rule cannot be converted to a CNF rule since this is an internal
        -- algorithmic error. TODO: We might consider using more fine-grained
        -- types capturing the properties introduced by each step in the future.
        toCNF :: Rule v t -> (v, [Either (v, v) t])
        toCNF (var :-> [Left a, Left b]) = (var, [Left (a, b)])
        toCNF (var :-> [Right c])        = (var, [Right c])
        toCNF _                          = error "toCNF internal error"

        -- Putting them all together.
        (newS, rules') = fst . flip runState 0 $ do
            (s', startRule) <- start s
            binRules <- concat <$> mapM bin (startRule : rules)
            let delUnitRules = unit . del s' [] $ binRules
            termRules <- concat <$> mapM term delUnitRules
            pure (s', Map.fromListWith (<>) . map toCNF $ termRules)
