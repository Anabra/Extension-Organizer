{-# LANGUAGE FlexibleContexts,
             TypeFamilies,
             RankNTypes
             #-}

module ExtensionOrganizer
  ( module ExtensionOrganizer
  , module ExtMonad
  ) where

import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.Refactor

import GHC
import GHC.LanguageExtensions.Type
import SrcLoc (RealSrcSpan, SrcSpan)

import Data.Ix
import Data.List
import Data.Char
import qualified Data.Set as Set
import qualified Data.Map.Strict as SMap
import Control.Monad.State
import Control.Reference ((^.), (&), (!~), (.-), (&+&), (^?), biplateRef)

import Debug.Trace (trace, traceShow)

import ExtMonad
import TraverseAST
import RecordWildCardsChecker
import FlexibleInstancesChecker
import DerivingsChecker

{-# ANN module "HLint: ignore Use mappend" #-}
{-# ANN module "HLint: ignore Redundant lambda" #-}

-- NOTE: When working on the entire AST, we should build a monad,
--       that will will avoid unnecessary checks.
--       For example if it already found a record wildcard, it won't check again


tryOut :: String -> String -> IO ()
tryOut = tryRefactor (localRefactoring . const organizeExtensions)


organizeExtensions :: ExtDomain dom => LocalRefactoring dom
organizeExtensions = \moduleAST -> do
  exts <- liftGhc $ collectExtensions moduleAST
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
                     UnnamedModule dom ->
                     Ghc ExtMap
collectExtensions = \moduleAST -> do
  let defaults = collectDefaultExtensions moduleAST
  flip execStateT SMap.empty . flip runReaderT defaults  . traverseModule $ moduleAST



collectDefaultExtensions :: UnnamedModule dom -> [Extension]
collectDefaultExtensions = map toExt . getExtensions
  where
  getExtensions :: UnnamedModule dom -> [String]
  getExtensions = flip (^?) (filePragmas & annList & lpPragmas & annList & langExt)

  toExt :: String -> Extension
  toExt = (read :: String -> Extension) . takeWhile isAlpha
