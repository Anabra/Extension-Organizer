{-# LANGUAGE FlexibleContexts,
             TypeFamilies #-}

module ExtensionOrganizer where

import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.Refactor

import GHC.LanguageExtensions.Type

import SrcLoc (RealSrcSpan, SrcSpan)

import Data.Ix
import Data.List
import qualified Data.Set as Set
import qualified Data.Map.Strict as SMap
import Control.Monad.State
import Control.Reference ((^.), (&), (!~), (.-), biplateRef)

import Debug.Trace (trace, traceShow)

import ExtMonad
import RecordWildCardsChecker
import FlexibleInstancesChecker
import DerivingsChecker

{-# ANN module "HLint: ignore Use mappend" #-}

-- NOTE: When working on the entire AST, we should build a monad,
--       that will will avoid unnecessary checks.
--       For example if it already found a record wildcard, it won't check again


tryOut :: String -> String -> IO ()
tryOut = tryRefactor (localRefactoring . collectExtensions)


-- NOTE: We will need a read-only reference
-- NOTE: Need to understand (Getter/Setter `op` f) types
collectExtensions :: ExtDomain dom => RealSrcSpan -> LocalRefactoring dom
collectExtensions sp = \moduleAST -> do
  (res, exts) <- flip runStateT SMap.empty . runAllChecks sp $ moduleAST
  let xs = SMap.assocs exts
  forM_ xs (\(ext, loc) -> do
    traceShow ext $ return ()
    forM loc (\l ->
      traceShow l $ return ()
      )
    )
  return res
  where runAllChecks sp = chkRecordWildCards sp
                          >=> chkFlexibleInstances sp
                          >=> (modDecl & annList !~ chkDerivings)
