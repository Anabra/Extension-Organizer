{-# LANGUAGE PatternSynonyms #-}

module SimpleTest where

pattern X a b c <- (a,b,c) {-* PatternSynonyms *-}
