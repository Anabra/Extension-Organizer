{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

-- NOTE: This module doesn't compile without TypeFamiles of GADTs
-- NOTE: This module doesn't compile without FlexibleContexts

-- Both errors come from inferred types -> will be hard to detect

module Example1 where

import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.Refactor

import GHC.LanguageExtensions.Type

import SrcLoc (RealSrcSpan, SrcSpan)

import qualified Data.Set as Set
import Control.Monad.State
import Control.Reference ((^.), (!~), (.-), biplateRef)


type ExtMonad dom = StateT (Set.Set Int) (LocalRefactor dom)

chkRecordWildCards sp = (nodesContained sp !~ chkRecordWildCardsPat)

chkRecordWildCardsPat :: PatternField dom -> ExtMonad dom (PatternField dom)
chkRecordWildCardsPat p@(FieldWildcardPattern _) = modify (Set.insert 0) >> return p
chkRecordWildCardsPat p = return p
