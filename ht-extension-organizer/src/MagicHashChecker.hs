{-# LANGUAGE FlexibleContexts #-}

module MagicHashChecker where

import ExtMonad
import Language.Haskell.Tools.Refactor

{-# ANN module "HLint: ignore Redundant bracket" #-}

chkMagicHashLiteral :: CheckNode Literal
chkMagicHashLiteral l@(PrimIntLit _)    = addOccurence MagicHash l
chkMagicHashLiteral l@(PrimWordLit _)   = addOccurence MagicHash l
chkMagicHashLiteral l@(PrimFloatLit _)  = addOccurence MagicHash l
chkMagicHashLiteral l@(PrimDoubleLit _) = addOccurence MagicHash l
chkMagicHashLiteral l@(PrimCharLit _)   = addOccurence MagicHash l
chkMagicHashLiteral l@(PrimStringLit _) = addOccurence MagicHash l


chkMagicHashNamePart :: CheckNode NamePart
chkMagicHashNamePart n@(NamePart name) =
  if (last name == '#') then addOccurence MagicHash n
                        else return n

{- Name can be reached from:
  UIESpec
  USubSpec
  UInjectivityAnn
  URuleVar
  UTopLevelPragma
  UAnnotationSubject
  UMinimalFormula

  UDecl
  UClassElement
  UDeclHead
  UGadtConDecl
  UPatSynLhs
  UPatternTypeSignature
  UFunDep
  UConDecl
  UFieldDecl
  UInstanceHead
  UTypeSignature
  UMatchLhs
  UTyVar
  UType
  UKind
  UAssertion
  UExpr
  UFieldUpdate
  UPattern
  UPatternField
  USplice
  UQuasiQuote

  UPromoted t
-}

{- QualifiedName can be reached from:
  UDecl
  UOperator
  UName (obviously)
-}
