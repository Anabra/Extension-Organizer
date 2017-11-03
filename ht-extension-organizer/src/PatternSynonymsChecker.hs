{-# LANGUAGE FlexibleContexts #-}

module PatternSynonymsChecker where

import ExtMonad
import Language.Haskell.Tools.Refactor

-- NOTE: Here we implicitly constrained the type with ExtDomain.
--       but we don't really need any.

chkPatternSynonymsSyn :: CheckNode PatternSynonym
chkPatternSynonymsSyn = conditional chkPatternSynonymsSyn' PatternSynonyms

chkPatternSynonymsSyn' :: CheckNode PatternSynonym
chkPatternSynonymsSyn' = addOccurence PatternSynonyms

chkPatternSynonymsTypeSig :: CheckNode PatternSignature
chkPatternSynonymsTypeSig = conditional (addOccurence PatternSynonyms) PatternSynonyms
