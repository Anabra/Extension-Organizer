{-# LANGUAGE FlexibleContexts #-}

module UnboxedTuplesChecker where

import ExtMonad
import Language.Haskell.Tools.Refactor

chkUnboxedTuplesExpr :: CheckNode Expr
chkUnboxedTuplesExpr = conditional chkUnboxedTuplesExpr' UnboxedTuples

chkUnboxedTuplesPat :: CheckNode Pattern
chkUnboxedTuplesPat = conditional chkUnboxedTuplesPat' UnboxedTuples

chkUnboxedTuplesType :: CheckNode Type
chkUnboxedTuplesType = conditional chkUnboxedTuplesType' UnboxedTuples


chkUnboxedTuplesExpr' :: CheckNode Expr
chkUnboxedTuplesExpr' e@(UnboxedTuple        _) = addOccurence UnboxedTuples e
chkUnboxedTuplesExpr' e@(UnboxedTupleSection _) = addOccurence UnboxedTuples e
chkUnboxedTuplesExpr' e = return e

chkUnboxedTuplesPat' :: CheckNode Pattern
chkUnboxedTuplesPat' p@(UnboxTuplePat _) = addOccurence UnboxedTuples p
chkUnboxedTuplesPat' p = return p

chkUnboxedTuplesType' :: CheckNode Type
chkUnboxedTuplesType' t@(UnboxedTupleType _) = addOccurence UnboxedTuples t
chkUnboxedTuplesType' t = return t
