module Parentable where

class Parentable a where
  mkParen  :: a -> a
  rmParen  :: a -> a

  isAtom :: a -> Bool

  rmParens :: a -> a
  rmParens x
    | isAtom x = x
    | otherwise = rmParens . rmParen $ x
