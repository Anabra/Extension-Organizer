{-# LANGUAGE FlexibleContexts,
             TypeFamilies
             #-}

module RecordWildCardsChecker where

import ExtMonad
import Control.Reference ((!~), (&), biplateRef)
import Language.Haskell.Tools.Refactor

--chkRecordWildCards sp = (biplateRef !~ chkRecordWildCardsPat)
--                          >=> (biplateRef !~ chkRecordWildCardsExpr)

chkRecordWildCardsPat :: CheckNode Pattern
chkRecordWildCardsPat = patternFields & annList !~ chkRecordWildCardsPatField

chkRecordWildCardsPatField :: CheckNode PatternField
chkRecordWildCardsPatField p@(FieldWildcardPattern x) = addOccurenceM RecordWildCards x >> return p
chkRecordWildCardsPatField p = return p

chkRecordWildCardsExpr :: CheckNode Expr
chkRecordWildCardsExpr = exprRecFields & annList !~ chkRecordWildCardsFieldUpdate

chkRecordWildCardsFieldUpdate :: CheckNode FieldUpdate
chkRecordWildCardsFieldUpdate e@(FieldWildcard x) = addOccurenceM RecordWildCards x >> return e
chkRecordWildCardsFieldUpdate e = return e
