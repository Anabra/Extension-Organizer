{-# LANGUAGE FlexibleContexts,
             FlexibleInstances,
             StandaloneDeriving,
             ConstraintKinds,
             RankNTypes
             #-}

module ExtMonad
  ( module ExtMonad
  , module ExtMap
  , module GHC.LanguageExtensions.Type
  , module Control.Monad.State
  , module Control.Monad.Reader
  ) where

import Language.Haskell.Tools.Refactor

import GHC
import GHC.Paths ( libdir )
import GHC.LanguageExtensions.Type
import SrcLoc (RealSrcSpan, SrcSpan)

import qualified Data.Set as Set
import qualified Data.Map.Strict as SMap
import Control.Monad.State
import Control.Monad.Reader

import ExtMap


{-# ANN module "HLint: ignore Use mappend" #-}
{-# ANN module "HLint: ignore Use import/export shortcut" #-}

deriving instance Ord  Extension
deriving instance Read Extension


-- how could I hide the tyvar a?
-- type Asd a = forall m . (MonadReader [Extension] m, MonadState ExtMap m, GhcMonad m) => m a


type ExtMonad        = ReaderT [Extension] (StateT ExtMap Ghc)
type ExtDomain dom   = (HasNameInfo dom)

type CheckNode elem  = forall dom . ExtDomain dom => elem dom -> ExtMonad (elem dom)
type CheckUNode uelem = forall dom . ExtDomain dom => Ann uelem dom SrcTemplateStage -> ExtMonad (Ann uelem dom SrcTemplateStage)

addOccurence :: (Ord k, HasRange a) =>
                k -> a -> SMap.Map k [SrcSpan] -> SMap.Map k [SrcSpan]
addOccurence key node = SMap.insertWith (++) key [getRange node]

-- TODO: add isTurnedOn check
addOccurenceM :: (MonadState ExtMap m, HasRange v) =>
                 Extension -> v -> m ()
addOccurenceM extension element = modify $ addOccurence (LVar extension) element

isTurnedOn :: Extension -> ExtMonad Bool
isTurnedOn ext = do
  defaults <- ask
  return $! ext `elem` defaults

runExtMonadIO :: ExtMonad a -> IO a
runExtMonadIO = runGhc (Just libdir) . runExtMonadGHC

runExtMonadGHC :: ExtMonad a -> Ghc a
runExtMonadGHC = liftM fst . flip runStateT SMap.empty . flip runReaderT []
