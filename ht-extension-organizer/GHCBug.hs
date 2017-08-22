{-# LANGUAGE DeriveAnyClass,
             MultiParamTypeClasses
             #-}

module DerivingsTest where

import Data.Data
import Data.Typeable
import Data.Generics
import Data.Ix

class C2 a b where
  f2 :: a -> b -> ()

data T0 = T0
  deriving (C2 Int)
