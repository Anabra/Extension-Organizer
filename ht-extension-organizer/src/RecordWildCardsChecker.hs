{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module RecordWildCardsChecker where

import ExtMonad
import Control.Reference ((!~))
import Language.Haskell.Tools.Refactor

chkRecordWildCards sp = (nodesContained sp !~ chkRecordWildCardsPat)
                          >=> (nodesContained sp !~ chkRecordWildCardsExpr)

chkRecordWildCardsPat :: PatternField dom -> ExtMonad dom (PatternField dom)
chkRecordWildCardsPat p@(FieldWildcardPattern x) = addOccurenceM RecordWildCards x >> return p
chkRecordWildCardsPat p = return p

chkRecordWildCardsExpr :: FieldUpdate dom -> ExtMonad dom (FieldUpdate dom)
chkRecordWildCardsExpr e@(FieldWildcard x) = addOccurenceM RecordWildCards x >> return e
chkRecordWildCardsExpr e = return e
