{-# LANGUAGE DeriveFunctor #-}

module ExtMap where

import GHC.LanguageExtensions.Type
import SrcLoc (RealSrcSpan, SrcSpan)

import qualified Data.Map.Strict as SMap


infix 6 :||:
infix 7 :&&:

data LogicalRelation a = LVar a
                       | Not (LogicalRelation a)
                       | LogicalRelation a :&&: LogicalRelation a
                       | LogicalRelation a :||: LogicalRelation a
  deriving (Eq, Show, Functor, Ord)

type ExtMap = SMap.Map (LogicalRelation Extension) [SrcSpan]
