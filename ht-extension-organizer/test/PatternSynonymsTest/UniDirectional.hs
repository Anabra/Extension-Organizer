{-# LANGUAGE PatternSynonyms #-}

module UniDirectional where

pattern X a b c <- (a,b,c) {-* PatternSynonyms *-}
