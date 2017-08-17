{-# LANGUAGE FlexibleContexts, StandaloneDeriving, ConstraintKinds #-}

module ExtMonad
  ( module ExtMonad
  , module GHC.LanguageExtensions.Type
  , module Control.Monad.State
  ) where

import Language.Haskell.Tools.Refactor

import GHC.LanguageExtensions.Type
import SrcLoc (RealSrcSpan, SrcSpan)

import qualified Data.Set as Set
import qualified Data.Map.Strict as SMap
import Control.Monad.State
import Control.Monad.Identity

{-# ANN module "HLint: ignore Use mappend" #-}

deriving instance Ord Extension


type ExtMonad  dom   = State (SMap.Map Extension [SrcSpan])
type ExtMonadT dom m = StateT (SMap.Map Extension [SrcSpan]) m
type ExtDomain dom   = (HasNameInfo dom)

addOccurence :: (HasRange v, Ord k) =>
                k -> v -> SMap.Map k [SrcSpan] -> SMap.Map k [SrcSpan]
addOccurence key node = SMap.insertWith f key [getRange node]
  where f range old = range ++ old


addOccurenceM extension element = modify $ addOccurence extension element
