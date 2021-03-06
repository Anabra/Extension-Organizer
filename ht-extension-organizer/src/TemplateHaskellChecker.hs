{-# LANGUAGE FlexibleContexts #-}

module TemplateHaskellChecker where

import ExtMonad
import Language.Haskell.Tools.Refactor

-- NOTE: Here we implicitly constrained the type with ExtDomain.
--       but we don't really need any.

-- can be reached from: Decl, Type, Expr, Pattern
chkTemplateHaskellSplice :: CheckNode Splice
chkTemplateHaskellSplice = addOccurence TemplateHaskell

-- can be reached from: Type, Expr, Pattern
chkTemplateHaskellQuasiQuote :: CheckNode QuasiQuote
chkTemplateHaskellQuasiQuote = addOccurence QuasiQuotes

-- can be reached from: Expr
chkTemplateHaskellBracket :: CheckNode Bracket
chkTemplateHaskellBracket = addOccurence TemplateHaskellQuotes
