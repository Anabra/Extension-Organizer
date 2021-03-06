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
import Language.Haskell.Tools.Daemon.MapExtensions

import GHC
import GHC.LanguageExtensions.Type
import SrcLoc (RealSrcSpan, SrcSpan)

import Data.List
import Data.Char
import qualified Data.Set as Set
import qualified Data.Map.Strict as SMap
import Control.Monad.State
import Control.Reference ((^.), (&), (!~), (.-), (.=), (&+&), (^?), biplateRef, _1, _2, _3)
import Control.Reference.Representation

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


organizeExtensionsRefactoring :: (HasModuleInfo dom, ExtDomain dom) => RefactoringChoice dom
organizeExtensionsRefactoring = ModuleRefactoring "OrganizeExtensions" (localRefactoring organizeExtensions)


tryOut :: String -> String -> IO ()
tryOut = tryRefactor (localRefactoring . const organizeExtensions)


organizeExtensions :: ExtDomain dom => LocalRefactoring dom
organizeExtensions = \moduleAST -> do
  exts <- liftGhc $ collectExtensions moduleAST
  let exts'      = calcExts exts
      newPragmas = [mkLanguagePragma . map show $ exts']
  return $ (filePragmas & annListElems .= newPragmas) moduleAST
  where isLVar (LVar _) = True
        isLVar _        = False

        calcExts :: ExtMap -> [Extension]
        calcExts logRels
          | ks <- SMap.keys logRels
          , all isLVar ks = map (\(LVar x) -> x) . SMap.keys $ logRels
          | otherwise     = []

        -- xs :: [(k, [v])]
        printExts xs = forM_ xs (\(ext, loc) -> do
                         traceShow ext $ return ()
                         forM loc (\l ->
                           traceShow l $ return ()
                           )
                         )




collectExtensions :: ExtDomain dom =>
                     UnnamedModule dom ->
                     Ghc ExtMap
collectExtensions = \moduleAST -> do
  let defaults = collectDefaultExtensions moduleAST
      expanded = concatMap expandExtension defaults
  flip execStateT SMap.empty . flip runReaderT expanded . traverseModule $ moduleAST


collectDefaultExtensions :: UnnamedModule dom -> [Extension]
collectDefaultExtensions = map toExt . getExtensions
  where
  getExtensions :: UnnamedModule dom -> [String]
  getExtensions = flip (^?) (filePragmas & annList & lpPragmas & annList & langExt)

  toExt :: String -> Extension
  toExt = (read :: String -> Extension) . takeWhile isAlpha


expandExtension :: Extension -> [Extension]
expandExtension ext = ext : implied
  where fst' = (^. _1) :: (a,b,c) -> a
        snd' = (^. _2) :: (a,b,c) -> b
        trd' = (^. _3) :: (a,b,c) -> c

        implied = map trd' . filter snd' . filter ((== ext) . fst') $ impliedXFlags
