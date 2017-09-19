{-# LANGUAGE BangPatterns #-}

module InStmt where

f = do
  (!a,_) <- Just (1,2)
  return a
