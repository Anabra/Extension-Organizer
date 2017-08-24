{-# LANGUAGE FlexibleContexts,
             FlexibleInstances,
             StandaloneDeriving,
             ConstraintKinds,
             TypeSynonymInstances #-}

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


type ExtMonad  dom   = StateT (SMap.Map Extension [SrcSpan]) (LocalRefactor dom)
type ExtDomain dom   = (HasNameInfo dom)

addOccurence :: (Ord k, HasRange a) =>
                k -> a -> SMap.Map k [SrcSpan] -> SMap.Map k [SrcSpan]
addOccurence key node = SMap.insertWith f key [getRange node]
  where f range old = range ++ old

addOccurenceM :: (MonadState (SMap.Map k [SrcSpan]) m, HasRange v, Ord k) =>
                 k -> v -> m ()
addOccurenceM extension element = modify $ addOccurence extension element


instance RefactorMonad (ExtMonad dom) where
  refactError = lift . refactError
  liftGhc = lift . liftGhc
