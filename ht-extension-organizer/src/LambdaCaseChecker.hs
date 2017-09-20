{-# LANGUAGE FlexibleContexts #-}

module LambdaCaseChecker where

import ExtMonad as Ext
import Language.Haskell.Tools.Refactor as Refact

chkLambdaCase :: CheckNode Expr
chkLambdaCase = conditional chkLambdaCase' Ext.LambdaCase

chkLambdaCase' :: CheckNode Expr
chkLambdaCase' e@(Refact.LambdaCase _) = addOccurence Ext.LambdaCase e
chkLambdaCase' e = return e


{-
  TopLevelPragma,
  Rule,

  Splice,
  Bracket,

  Cmd,

  Rhs DONE
  RhsGuard DONE
  Expr DONE
  FieldUpdate DONE
  TupSecElem DONE
  CaseRhs DONE
  Stmt DONE
  CompStmt DONE
  Pattern DONE
-}
