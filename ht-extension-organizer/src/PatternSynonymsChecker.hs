{-# LANGUAGE FlexibleContexts #-}

module PatternSynonymsChecker where

import ExtMonad
import Language.Haskell.Tools.Refactor

-- NOTE: Here we implicitly constrained the type with ExtDomain.
--       but we don't really need any.

chkPatternSynonyms :: CheckNode PatternSynonym
chkPatternSynonyms = conditional chkPatternSynonyms' PatternSynonyms

chkPatternSynonyms' :: CheckNode PatternSynonym
chkPatternSynonyms' = addOccurence PatternSynonyms
