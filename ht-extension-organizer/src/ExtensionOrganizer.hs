{-# LANGUAGE FlexibleContexts,
             TypeFamilies #-}

module ExtensionOrganizer where

import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.Refactor

import GHC
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
{-# ANN module "HLint: ignore Redundant lambda" #-}

-- NOTE: When working on the entire AST, we should build a monad,
--       that will will avoid unnecessary checks.
--       For example if it already found a record wildcard, it won't check again


tryOut :: String -> String -> IO ()
tryOut = tryRefactor (localRefactoring . organizeExtensions)


organizeExtensions :: ExtDomain dom => RealSrcSpan -> LocalRefactoring dom
organizeExtensions sp = \moduleAST -> do
  exts <- liftGhc $ collectExtensions sp moduleAST
  let xs = SMap.assocs exts
  forM_ xs (\(ext, loc) -> do
    traceShow ext $ return ()
    forM loc (\l ->
      traceShow l $ return ()
      )
    )
  return moduleAST

-- TODO: get rid of RealSrcSpan arguments
-- NOTE: We will need a read-only reference
-- NOTE: Need to understand (Getter/Setter `op` f) types
collectExtensions :: ExtDomain dom =>
                     RealSrcSpan ->
                     UnnamedModule dom ->
                     Ghc (SMap.Map Extension [SrcSpan])
collectExtensions sp = \moduleAST -> do
  (_, exts) <- flip runStateT SMap.empty . runAllChecks sp $ moduleAST
  return exts
  where runAllChecks sp = chkRecordWildCards sp
                          >=> (modDecl & annList !~ chkDecls)
        chkDecls = chkFlexibleInstances >=> chkDerivings
