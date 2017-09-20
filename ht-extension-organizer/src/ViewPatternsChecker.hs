{-# LANGUAGE FlexibleContexts #-}

module ViewPatternsChecker where

import ExtMonad
import Language.Haskell.Tools.Refactor

chkViewPatterns :: CheckNode Pattern
chkViewPatterns = conditional chkViewPatterns' ViewPatterns

chkViewPatterns' :: CheckNode Pattern
chkViewPatterns' p@(ViewPat _ _) = addOccurence ViewPatterns p
chkViewPatterns' p = return p
