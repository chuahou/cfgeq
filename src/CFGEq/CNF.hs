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

{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CFGEq.CNF ( CNF (..)
                 , CNFProduction
                 , compile
                 , enumerate
                 ) where

import           Control.Monad       (forM)
import           Control.Monad.State (State, get, put, runState)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Maybe          (fromMaybe, mapMaybe)
import           Data.Set            (Set)
import qualified Data.Set            as Set

import           CFGEq.CFG

-- | @CNF v t@ is the type of Chomsky normal form grammars with variables of
-- type @v@ and terminals of type @t@.
data CNF v t = CNF { rulesCNF      :: Map v (Set (CNFProduction v t))
                                           -- ^ Map of variables to their set of rules.
                   , startCNF      :: v    -- ^ The start symbol.
                   , producesEmpty :: Bool -- ^ Whether the start symbol produces epsilon.
                   }
    deriving Show

-- | The type of productions in a rule for 'CNF'. Does not allow epsilon
-- productions since 'CNF' captures that for the start symbol as a 'Bool'.
type CNFProduction v t = Either (v, v) t

-- | Cons operator for 'Set's for convenience.
(<:) :: Ord a => a -> Set a -> Set a
(<:) = Set.insert

-- | Compiles a 'CFG' to 'CNF'. Requires the caller to supply a function that
-- supplies fresh variables of the correct type, that should be guaranteed to
-- return a different variable when given a different 'Int' argument.
compile :: forall v t. (Ord v, Ord t)
        => (Int -> v) -- ^ Fresh variable supplier.
        -> CFG v t    -- ^ Grammar to compile.
        -> CNF v t
compile mkFresh (CFG rs s) = CNF rules' newS sEmpty
    where
        -- States here are used to call @mkFresh@ with fresh 'Int's every time
        -- we need a fresh variable, keeping track of the largest number we've
        -- used so far.

        -- START step
        -- Generates a rule with a new start symbol that produces the old start
        -- symbol @oldS@.
        startStep :: v -> State Int (v, Rule v t)
        startStep oldS = get >>= \gen -> put (succ gen) >> pure
            (let s' = mkFresh gen
              in (s', s' :-> [Left oldS]))

        -- BIN step
        -- Converts given rule to a list of equivalent rules each with <= 2
        -- variables on the right hand side.
        bin :: Rule v t -> State Int (Set (Rule v t))
        bin (var :-> (a:b:c:cs)) = do
            gen <- get
            let f = mkFresh gen
            put . succ $ gen
            recurse <- bin (f :-> (b:c:cs))
            pure $ (var :-> [a, Left f]) <: recurse
        bin r = pure [r]

        -- DEL step
        -- Eliminates nullable rules, giving the new list of rules, given the
        -- start symbol and list of already removed rules. At each iteration it
        -- finds a rule \(A \to \epsilon\), removes it, and changes every other
        -- rule containing \(A\) on the RHS.
        del :: v -> Set v -> Set (Rule v t) -> Set (Rule v t)
        del s' removed rs' =
            case Set.lookupMin $
                Set.filter (\(var :-> w) -> null w && var /= s') rs' of
              Nothing          -> rs'
              Just (var :-> _) -> del s' (var <: removed)
                                . Set.unions . Set.map (go var) $ rs'
            where
                -- Generate the list of corresponding new rules based on @var@
                -- being nullable.
                go :: v -> Rule v t -> Set (Rule v t)
                go var (var' :-> w)
                    | var == var' && null w = [] -- Remove this rule.
                    | otherwise = Set.map (var' :->)
                                    -- Remove any added epsilon rule if it's
                                    -- been removed before.
                                . (if var' `elem` (var <: removed)
                                      then Set.delete []
                                      else id)
                                $ go' var w
                go' :: v -> [Either v t] -> Set [Either v t]
                go' _ [] = [[]]
                go' var (Left v:w)
                    | var == v  = next <> Set.map (Left v:) next
                    | otherwise = Set.map (Left v:) next
                    where next = go' var w
                go' var (Right t:w) = Set.map (Right t:) $ go' var w

        -- UNIT step
        -- Eliminates unit rules of form \(A \to B\), given a set of already
        -- removed unit rules.
        unit :: Set (Rule v t) -> Set (Rule v t) -> Set (Rule v t)
        unit removed s' =
            case Set.lookupMin $
                Set.filter (\(_ :-> w) -> case w of
                                            [Left _] -> True
                                            _        -> False) s' of
                Nothing                  -> s'
                Just r@(v :-> [Left v']) -> unit (r <: removed)
                                          . Set.unions . Set.map (go v v') $ s'
                _                        -> error "Unreachable 'unit'"
            where
                -- Performs relevant changes for unit rule \(v \to v'\) on rule
                -- @r@. Removes \(v \to v'\) and replaces all \(v' \to w\) with
                -- both \(v \to w\) and \(v' \to w\).
                go :: v -> v -> Rule v t -> Set (Rule v t)
                go v v' r@(u :-> w)
                    -- Remove the current rule.
                    | v == u && w == [Left v'] = []
                    -- Don't add a rule that's already removed.
                    | v' == u && not (Set.member (v :-> w) removed)
                                               = [v :-> w, r]
                    | otherwise                = [r]

        -- TERM step
        -- Converts given rule into one with no nonsolitary terminals.
        term :: Set (Rule v t) -> State Int (Set (Rule v t))
        term s' = termVars >>= \termVars' -> pure $ Set.map (go termVars') s'

                    -- New rules corresponding to @termVars'@.
                    `Set.union` Set.map (\(t, v) -> v :-> [Right t])
                                    (Set.fromList . Map.toList $ termVars')

            where
                -- Extract list of unique terminals.
                terminals :: [t]
                terminals = Set.toList . Set.unions . Set.map go' $ s'
                    where
                        go' (_ :-> w) = Set.fromList $
                            mapMaybe (either (const Nothing) Just) w

                -- Assign each terminal to a fresh variable.
                termVars :: State Int (Map t v)
                termVars = fmap Map.fromList . forM terminals $ (\t -> do
                    gen <- get
                    put . succ $ gen
                    pure (t, mkFresh gen))

                -- Replace all non-unit occurrences of terminals with the fresh
                -- variable.
                go :: Map t v -> Rule v t -> Rule v t
                go m (v :-> p@[_, _]) = v :-> map go' p
                    where go' = either Left (Left . (m Map.!))
                go _ r = r

        -- Converts a CFG rule into a CNF rule. Errors impurely when the CFG
        -- rule cannot be converted to a CNF rule since this is an internal
        -- algorithmic error. TODO: We might consider using more fine-grained
        -- types capturing the properties introduced by each step in the future.
        toCNF :: Rule v t -> (v, Set (Either (v, v) t))
        toCNF (var :-> [Left a, Left b]) = (var, [Left (a, b)])
        toCNF (var :-> [Right c])        = (var, [Right c])
        toCNF _                          = error "toCNF internal error"

        -- Putting them all together.
        (newS, rules', sEmpty) = fst . flip runState 0 $ do
            (s', startRule) <- startStep s
            binRules        <- Set.unions <$> mapM bin (Set.toList (startRule <: rs))
            let delUnitRules = unit [] . del s' [] $ binRules
            termRules       <- term delUnitRules
            let sEmpty'      = Set.member (s' :-> []) termRules
            let noEmptyRules = Set.delete (s' :-> []) termRules
            pure ( s'
                 , Map.fromListWith (<>) . map toCNF . Set.toList $ noEmptyRules
                 , sEmpty'
                 )

-- | @enumerate n c@ returns the set of all strings generated by @c@ of length
-- @n@ or less.
enumerate :: forall v t. (Ord v, Ord t)
          => Int     -- ^ Maximum length of string to generate.
          -> CNF v t -- ^ Chomsky normal form grammar to use.
          -> Set [t]
enumerate n (CNF r s e) = (if e then Set.singleton [] else Set.empty)
        `Set.union` go n [Left s]

    where
        -- Generate all possible strings of terminals of less than of equal the
        -- given length from the given string.
        go :: Int -> [Either v t] -> Set [t]
        go _  []          = Set.singleton []
        go n' (Right t:w) = Set.map (t:) $ go (n' - 1) w
        go n' (Left v:w)  = Set.unions . Set.map (go n') $ vprods'
            where
                -- Possibilities that @v@ can become.
                vprods :: Set (CNFProduction v t)
                vprods = fromMaybe Set.empty $ Map.lookup v r

                -- Make possibilities @vprods@ into a set of resulting strings
                -- after applying each possibility, then restrict them to the
                -- given length.
                vprods' :: Set [Either v t]
                vprods' = Set.filter ((<= n') . length) . Set.map go' $ vprods
                    where
                        go' (Left (v1, v2)) = Left v1 : Left v2 : w
                        go' (Right t)       = Right t : w
