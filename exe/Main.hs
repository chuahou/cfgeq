-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module Main (main) where

import qualified Data.Set           as Set
import           System.Environment (getArgs, getProgName)
import           System.IO          (hPrint, hPutStrLn, stderr)
import           Text.Parsec.String (parseFromFile)
import           Text.Read          (readMaybe)

import           CFGEq.CFG          (CFG)
import           CFGEq.CNF          (compile, enumerate)
import           Control.Monad      (forM_)
import           Parser             (fileP)

-- | Entry point. Expects exactly 2 arguments (filename to parse and length to
-- enumerate till).
main :: IO ()
main = getArgs >>= \case
    [f, n] -> case readMaybe n :: Maybe Int of
                Just n' -> parseFromFile fileP f >>= \case
                    Left err       -> hPrint stderr err
                    Right (c1, c2) -> runCompare c1 c2 n'
                Nothing -> usage >>= hPutStrLn stderr
    _      -> usage >>= hPutStrLn stderr

-- | Usage string.
usage :: IO String
usage = getProgName >>= \prog -> pure $ "Usage: " <> prog <> " [file] [length]"

-- | Run checks up to given enumeration length and print results.
runCompare :: CFG String String -> CFG String String -> Int -> IO ()
runCompare c1 c2 n = case compareCFGs c1 c2 n of
    Nothing -> putStrLn $ "No counterexample found up to length " <> show n
    Just (ce1, ce2) -> do
        forM_ ce1 (\ce -> putStrLn $
            "Counterexample in 1st language but not in 2nd:" <> show ce)
        forM_ ce2 (\ce -> putStrLn $
            "Counterexample in 2nd language but not in 1st:" <> show ce)

-- | Run checks up to given enumeration length, returning @Just@ a tuple of
-- counterexamples if found, and @Nothing@ otherwise.
compareCFGs :: CFG String String -> CFG String String
            -> Int -> Maybe (Maybe [String], Maybe [String])
compareCFGs c1 c2 n =
    -- Fresh variable supplier. No variables will have a space in them so we
    -- append a space to numbers.
    let mkFresh = (map ((<> " ") . show) ([0..] :: [Integer]) !!)
        ss1 = enumerate n $ compile mkFresh c1
        ss2 = enumerate n $ compile mkFresh c2
     in if ss1 == ss2 then Nothing
        else Just (Set.lookupMin $ ss1 Set.\\ ss2, Set.lookupMin $ ss2 Set.\\ ss1)
