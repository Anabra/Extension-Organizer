{-# LANGUAGE FlexibleContexts,
             RankNTypes
             #-}

module TraverseAST
  ( module TraverseAST
  , module ExtMonad
  ) where

import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.Refactor as Refact
import Language.Haskell.Tools.AST

import Control.Reference ((!~), (&), (&+&))

import Debug.Trace (trace)

import ExtMonad
import RecordWildCardsChecker
import FlexibleInstancesChecker
import DerivingsChecker
import BangPatternsChecker
import PatternSynonymsChecker
import TemplateHaskellChecker
import ViewPatternsChecker
import LambdaCaseChecker
import TupleSectionsChecker
import UnboxedTuplesChecker


chkDecl :: CheckNode Decl
chkDecl = chkFlexibleInstances >=> chkDerivings

chkPattern :: CheckNode Pattern
chkPattern = chkBangPatterns
         >=> chkViewPatterns
         >=> chkUnboxedTuplesPat

chkExpr :: CheckNode Expr
chkExpr = chkTupleSections
      >=> chkUnboxedTuplesExpr
      >=> chkLambdaCase

chkType :: CheckNode Type
chkType = chkUnboxedTuplesType

chkPatternField :: CheckNode PatternField
chkPatternField = chkRecordWildCardsPatField

chkFieldUpdate :: CheckNode FieldUpdate
chkFieldUpdate = chkRecordWildCardsFieldUpdate

chkPatternSynonym :: CheckNode PatternSynonym
chkPatternSynonym = chkPatternSynonyms


traverseModule :: CheckNode UnnamedModule
traverseModule = modDecl & annList !~ traverseDecl

traverseDecl :: CheckNode Decl
traverseDecl = chkDecl
               >=> (declValBind !~ traverseValueBind)
               >=> (declPatSyn !~ traversePatternSynonym)
               >=> (declBody & annJust !~ traverseClassBody)
               >=> (declInstDecl & annJust !~ traverseInstBody)
               >=> (declSplice !~ traverseSplice)
               -- needs more (but could solve it through the checkers)


traversePatternSynonym :: CheckNode PatternSynonym
traversePatternSynonym = chkPatternSynonym
                         >=> (patRhs !~ traverseInnerRhs)
                         >=> (patLhs !~ traverseInnerLhs)

  where traverseInnerRhs = (patRhsPat !~ traversePattern)
          >=> (patRhsOpposite & annJust & patOpposite & annList !~ traverseMatch)

        traverseInnerLhs = (innerName !~ traverseName)
                           >=> (patSynOp !~ traverseOperator)

        innerName = patName &+& (patArgs & annList) &+& patSynLhs &+& patSynRhs



traverseMatch :: CheckNode Match
traverseMatch = (matchLhs !~ traverseMatchLhs)
                >=> (matchRhs !~ traverseRhs)
                >=> (matchBinds & annJust !~ traverseLocalBinds)

traverseMatchLhs :: CheckNode MatchLhs
traverseMatchLhs = (matchLhsName !~ traverseName)
                   >=> (matchLhsOperator !~ traverseOperator)
                   >=> (innerPattern !~ traversePattern)

  where innerPattern = matchLhsArgs & annList
                       &+& matchLhsLhs
                       &+& matchLhsRhs

traverseRhs :: CheckNode Rhs
traverseRhs = (rhsExpr !~ traverseExpr)
              >=> (rhsGuards & annList !~ traverseGuardedRhs)

traverseGuardedRhs :: CheckNode GuardedRhs
traverseGuardedRhs = (guardStmts & annList !~ traverseRhsGuard)
                     >=> (guardExpr !~ traverseExpr)

traverseRhsGuard :: CheckNode RhsGuard
traverseRhsGuard = (guardPat !~ traversePattern)
                   >=> (guardRhs &+& guardCheck !~ traverseExpr)
                   >=> (guardBinds & annList !~ traverseLocalBind)

traverseLocalBinds :: CheckNode LocalBinds
traverseLocalBinds = localBinds & annList !~ traverseLocalBind

traverseLocalBind :: CheckNode LocalBind
traverseLocalBind = (localVal !~ traverseValueBind)
                    >=> (localSig !~ traverseTypeSignature)
                    >=> (localFixity !~ traverseFixitySignature)
                    -- >=> (localInline !~ ...)


traverseInstBody :: CheckNode InstBody
traverseInstBody = instBodyDecls & annList !~ traverseInstBodyDecl

traverseInstBodyDecl :: CheckNode InstBodyDecl
traverseInstBodyDecl = (instBodyDeclFunbind !~ traverseValueBind)
                       >=> (instBodyTypeSig !~ traverseTypeSignature)
                       >=> (instBodyTypeEqn !~ traverseTypeEqn)
                       >=> (specializeInstanceType !~ traverseType)
                       -- and many more ...

traverseTypeEqn :: CheckNode TypeEqn
traverseTypeEqn = teLhs &+& teRhs !~ traverseType

traverseClassBody :: CheckNode ClassBody
traverseClassBody = cbElements & annList !~ traverseClassElem

traverseClassElem :: CheckNode ClassElement
traverseClassElem = (ceTypeSig !~ traverseTypeSignature)
                    >=> (clsFixity !~ traverseFixitySignature)
                    >=> (ceBind !~ traverseValueBind)
                    >=> (ceName !~ traverseName)
                    >=> (ceKind &+& ceType !~ traverseType)

                    -- some more, but are not important


traverseValueBind :: CheckNode ValueBind
traverseValueBind = (valBindPat !~ traversePattern)
                    >=> (valBindRhs !~ traverseRhs)
                    >=> (valBindLocals & annJust !~ traverseLocalBinds)
                    >=> (funBindMatches & annList !~ traverseMatch)


traversePattern :: CheckNode Pattern
traversePattern = chkPattern
                  >=>(innerPattern !~ traversePattern)
                  >=> (innerLiteral !~ traverseLiteral)
                  >=> (patternOperator !~ traverseOperator)
                  >=> (patternFields & annList !~ traversePatternField)
                  >=> (patternName !~ traverseName)
                  >=> (patternExpr !~ traverseExpr)
                  >=> (patternType !~ traverseType)
                  >=> (patternSplice !~ traverseSplice)
                  >=> (patQQ !~ traverseQuasiQuote)

  where
  innerPattern = patternLhs
                 &+& patternRhs
                 &+& patternInner
                 &+& (patternElems & annList)
                 &+& (patternArgs & annList)

  innerLiteral = patternLiteral &+& patternLit

traversePatternField :: CheckNode PatternField
traversePatternField = chkPatternField
                       >=> (fieldPatternName !~ traverseName)
                       >=> (fieldPattern !~ traversePattern)

-- TODO: TemplateHaskell?
traverseExpr :: CheckNode Expr
traverseExpr = chkExpr
              >=> (innerExpressions !~ traverseExpr)
              >=> (innerPatterns !~ traversePattern)
              >=> (exprFunBind & annList !~ traverseLocalBind)
              >=> (exprIfAlts & annList !~ traverseGuardedCaseRhs traverseExpr)
              >=> (exprAlts & annList !~ traverseAlt traverseExpr)
              >=> (exprOperator !~ traverseOperator)
              >=> (exprLit !~ traverseLiteral)
              >=> (innerNames !~ traverseName)
              >=> (exprStmts & annList !~ traverseStmt traverseExpr)
              >=> (tupleSectionElems & annList !~ traverseTupSecElem)
              >=> (exprRecFields & annList !~ traverseFieldUpdate)
              >=> (compBody & annList !~ traverseListCompBody)
              >=> (innerTypes !~ traverseType)
              >=> (exprSplice !~ traverseSplice)
              >=> (exprBracket !~ traverseBracket)
              >=> (exprQQ !~ traverseQuasiQuote)

 where innerExpressions = (tupleElems & annList)
                         &+& (listElems & annList)
                         &+& innerExpr
                         &+& exprCond
                         &+& exprThen
                         &+& exprElse
                         &+& exprRhs
                         &+& exprLhs
                         &+& exprInner
                         &+& exprFun
                         &+& exprCase
                         &+& exprArg
                         &+& enumToFix
                         &+& (enumTo & annJust)
                         &+& (enumThen & annJust)
                         &+& Refact.enumFrom
                         &+& compExpr

       innerPatterns = (exprBindings & annList) &+& procPattern
       innerNames    = exprName &+& exprRecName &+& quotedName
       innerTypes    = exprSig &+& exprType

-- These types are needed to generalize AST traversal for polymorphic nodes.
-- These nodes are polymorphic in their inner nodes, which can be any "UNodes"
-- (UType. UExpr, UDecl etc ...)
type AltG uexpr dom            = Ann (UAlt' uexpr) dom SrcTemplateStage
type StmtG uexpr dom           = Ann (UStmt' uexpr) dom SrcTemplateStage
type CaseRhsG uexpr dom        = Ann (UCaseRhs' uexpr) dom SrcTemplateStage
type GuardedCaseRhsG uexpr dom = Ann (UGuardedCaseRhs' uexpr) dom SrcTemplateStage

type PromotedG t dom = Ann (UPromoted t) dom  SrcTemplateStage

traverseAlt :: CheckUNode uexpr -> CheckNode (AltG uexpr)
traverseAlt f = (altPattern !~ traversePattern)
               >=> (altRhs !~ traverseCaseRhs f)
               >=> (altBinds & annJust !~ traverseLocalBinds)

traverseCaseRhs :: CheckUNode uexpr -> CheckNode (CaseRhsG uexpr)
traverseCaseRhs f = (rhsCaseExpr !~ f)
                   >=> (rhsCaseGuards & annList !~ traverseGuardedCaseRhs f)

traverseGuardedCaseRhs :: CheckUNode uexpr -> CheckNode (GuardedCaseRhsG uexpr)
traverseGuardedCaseRhs f = (caseGuardStmts & annList !~ traverseRhsGuard)
                          >=> (caseGuardExpr !~ f)

traverseStmt :: CheckUNode uexpr -> CheckNode (StmtG uexpr)
traverseStmt f = (stmtPattern !~ traversePattern)
                >=> (stmtExpr !~ f)
                >=> (stmtBinds & annList !~ traverseLocalBind)
                >=> (cmdStmtBinds & annList !~ traverseStmt f)

traversePromoted :: CheckUNode t -> CheckNode (PromotedG t)
traversePromoted f = (promotedConName !~ traverseName)
                 >=> (promotedElements & annList !~ f)

traverseTupSecElem :: CheckNode TupSecElem
traverseTupSecElem = tupSecExpr !~ traverseExpr

traverseFieldUpdate :: CheckNode FieldUpdate
traverseFieldUpdate = chkFieldUpdate
                      >=> (fieldName &+& fieldUpdateName !~ traverseName)
                      >=> (fieldValue !~ traverseExpr)

traverseListCompBody :: CheckNode ListCompBody
traverseListCompBody = compStmts & annList !~ traverseCompStmt

traverseCompStmt :: CheckNode CompStmt
traverseCompStmt = (compStmt !~ traverseStmt traverseExpr)
                  >=> (innerExpressions !~ traverseExpr)

 where innerExpressions = thenExpr
                          &+& (byExpr & annJust)
                          &+& (usingExpr & annJust)

traverseCmd :: CheckNode Cmd
traverseCmd = return
{-

References are not generated for UCmd

traverseCmd = (innerExpressions !~ traverseExpr)
             >=> (innerCmds !~ traverseCmd)
             >=> (cmdStmtBinds & annList !~ traversePattern)
             >=> (cmdBinds & annList !~ traverseLocalBind)
             >=> (cmdAlts & annList !~ traverseAlt traverseCmd)
             >=> (cmdStmts & annList !~ traverseStmt traverseCmd)
 where innerExpressions = cmdLhs &+& cmdRhs &+& cmdExpr &+& cmdApplied

       innerCmds = (cmdInnerCmds & annList)
                   &+& cmdInnerCmd
                   &+& cmdLeftCmd
                   &+& cmdRightCmd
                   &+& cmdInner
                   &+& cmdThen
                   &+& cmdElse
-}

traverseSplice :: CheckNode Splice
traverseSplice = chkTemplateHaskellSplice
                 >=> (spliceId !~ traverseName)
                 >=> (spliceExpr !~ traverseExpr)

traverseBracket :: CheckNode Bracket
traverseBracket = chkTemplateHaskellBracket
                  >=> (bracketExpr !~ traverseExpr)
                  >=> (bracketPattern !~ traversePattern)
                  >=> (bracketType !~ traverseType)
                  >=> (bracketDecl & annList !~ traverseDecl)

traverseQuasiQuote :: CheckNode QuasiQuote
traverseQuasiQuote = chkTemplateHaskellQuasiQuote
                     >=> (qqExprName !~ traverseName)

traverseType :: CheckNode Type
traverseType = chkType
              >=> (typeBounded & annList !~ traverseTyVar)
              >=> (innerType !~ traverseType)
              >=> (innerName !~ traverseName)
              >=> (typeCtx !~ traverseContext)
              >=> (typeOperator !~ traverseOperator)
              >=> (typeKind !~ traverseKind)
              >=> (tpPromoted !~ traversePromoted traverseType)
              >=> (tsSplice !~ traverseSplice)
              >=> (typeQQ !~ traverseQuasiQuote)

 where innerType = typeType
                   &+& typeParam
                   &+& typeResult
                   &+& (typeElements & annList)
                   &+& typeElement
                   &+& typeCon
                   &+& typeArg
                   &+& typeInner
                   &+& typeLeft
                   &+& typeRight

       innerName = typeName &+& typeWildcardName

traverseTyVar :: CheckNode TyVar
traverseTyVar = (tyVarName !~ traverseName)
               >=> (tyVarKind & annJust !~ traverseKindContraint)

traverseKindContraint :: CheckNode KindConstraint
traverseKindContraint = kindConstr !~ traverseKind

traverseKind :: CheckNode Kind
traverseKind = return

traverseContext :: CheckNode Context
traverseContext = contextAssertion !~ traverseAssertion

traverseAssertion :: CheckNode Assertion
traverseAssertion = (innerType !~ traverseType)
                   >=> (innerName !~ traverseName)
                   >=> (assertOp !~ traverseOperator)
                   >=> (innerAsserts & annList !~ traverseAssertion)

 where innerType = (assertTypes & annList)
                   &+& assertLhs
                   &+& assertRhs
                   &+& assertImplType

       innerName = assertClsName &+& assertImplVar

-- TODO:

traverseTypeSignature :: CheckNode TypeSignature
traverseTypeSignature = return

traverseFixitySignature :: CheckNode FixitySignature
traverseFixitySignature = return

traverseName :: CheckNode Name
traverseName = return

traverseOperator :: CheckNode Operator
traverseOperator = return

traverseLiteral :: CheckNode Literal
traverseLiteral = return
