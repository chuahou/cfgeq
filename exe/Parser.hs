-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module Parser where

import           Control.Monad      (void)
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Text.Parsec        (alphaNum, char, choice, endBy1, many1,
                                     oneOf, optional, sepBy1, sepEndBy1, string,
                                     try, (<|>))
import           Text.Parsec.String (Parser)

import           CFGEq.CFG          (CFG (..), Production, Rule (..))

-- | A 'Parser' for the file format we use.
-- The file consists of a prelude declaring the start symbol and terminal
-- strings ended by '%%':
--
-- > %start expr
-- > %token IDENT LPAR RPAR
-- > %%
--
-- followed by 2 CFGs, separated with @%%@.
--
-- > expr -> IDENT | LPAR expr RPAR | expr expr;
-- >
-- > %%
-- >
-- > expr -> expr1 | expr expr1;
-- > expr1 -> IDENT | LPAR expr RPAR;
--
-- See the syntax of CFGs in 'cfgP'.
fileP :: Parser (CFG String String, CFG String String)
fileP = do
    (s, ts) <- wsP *> choice
        [ (,) <$> try startP <*> tokenP
        , flip (,) <$> try tokenP <*> startP
        ] <* sepP
    cfg1 <- cfgP ts s <* wsP <* sepP
    cfg2 <- cfgP ts s
    pure (cfg1, cfg2)

        where
            startP = string "%start" *> wsP' *> identP <* wsP'
            tokenP = string "%token" *> wsP' *> sepEndBy1 identP wsP'
            sepP = void $ string "%%" >> wsP

-- | A Parsec 'Parser' for 'CFG's with the type of both variables and terminals
-- being @String@. Requires provided terminal strings (all other strings are
-- considered variables) and a start symbol.
--
-- A CFG consists of a group of rules, each terminated with @;@. \(\to\) is
-- written @->@ and \(\mid\) is written @|@. For example,
--
-- > expr -> expr1 | expr expr1;
-- > expr1 -> IDENT | LPAR expr RPAR;
cfgP :: Foldable f
     => f String -- ^ Terminal strings.
     -> String   -- ^ Start symbol.
     -> Parser (CFG String String)
cfgP ts s = sepEndBy1 (try ruleP) wsP >>= \rs -> pure $ CFG (Set.unions rs) s
    where
        identP' :: Parser (Either String String)
        identP' = identP >>= \cs ->
            pure $ (if cs `elem` ts then Right else Left) cs

        varP :: Parser String
        varP = identP' >>= either pure (const $ fail "variable")

        prodsP :: Parser [Production String String]
        prodsP = sepBy1 prodP (char '|' <* wsP)

        prodP :: Parser (Production String String)
        prodP = (try (string "epsilon" <* wsP) >> pure [])
            <|> endBy1 identP' wsP

        ruleP :: Parser (Set (Rule String String))
        ruleP = do
            v <- varP <* wsP
            void $ string "->" <* wsP
            ps <- prodsP <* char ';'
            pure . Set.fromList $ map (v :->) ps

-- | Parses none or some whitespace.
wsP :: Parser ()
wsP = optional wsP'

-- | Parses at least some whitespace.
wsP' :: Parser ()
wsP' = void . many1 $ oneOf [' ', '\t', '\r', '\n']

-- | Parses an identifier acceptable as a variable of terminal.
identP :: Parser String
identP = many1 (alphaNum <|> oneOf "_")
