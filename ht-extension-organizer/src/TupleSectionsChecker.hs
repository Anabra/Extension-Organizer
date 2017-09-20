{-# LANGUAGE FlexibleContexts #-}

module TupleSectionsChecker where

import ExtMonad
import Language.Haskell.Tools.Refactor

chkTupleSections :: CheckNode Expr
chkTupleSections = conditional chkTupleSections' TupleSections

chkTupleSections' :: CheckNode Expr
chkTupleSections' e@(TupleSection        _) = addOccurence TupleSections e
chkTupleSections' e@(UnboxedTupleSection _) = addOccurence TupleSections e
                                           >> addOccurence UnboxedTuples e
chkTupleSections' e = return e
