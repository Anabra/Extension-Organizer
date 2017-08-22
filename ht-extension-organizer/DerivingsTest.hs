{-# LANGUAGE DeriveDataTypeable,
             DeriveGeneric,
             DeriveFunctor,
             DeriveFoldable,
             DeriveTraversable,
             DeriveAnyClass,
             GeneralizedNewtypeDeriving,
             MultiParamTypeClasses
             #-}

module DerivingsTest where

import Data.Data
import Data.Typeable
import Data.Generics
import Data.Ix

class C1 a where
  f1 :: a -> ()
  f1 _ = ()

class C2 a b where
  f2 :: a -> b -> ()
  f2 _ _ = ()

-- OK
data D1 a = D1 a
  deriving Show

-- OK
data D2 a = D2 a
  deriving Eq

-- DeriveDataTypeable
data D3 a = D3 a
  deriving Data

-- DeriveFunctor
data D4 a = D4 a
  deriving Functor

-- DeriveDataTypeable
-- DeriveFoldable
-- DeriveAnyClass
data D5 a = D5 a
  deriving (Eq, Ord, Typeable, Foldable, C1)

-- DeriveDataTypeable
-- DeriveFunctor
-- DeriveFoldable
-- DeriveTraversable
-- DeriveAnyClass
data D6 a = D6
  deriving (Eq, Ord, Enum, Ix, Bounded, Show, Read,
            Data, Typeable, Functor, Foldable, Traversable)

-- DeriveDataTypeable
newtype T1 a = T1 (D6 a)
  deriving Data

-- DeriveDataTypeable
newtype T2 a = T2 (D6 a)
  deriving (Show, Read, Data, Typeable)

-- GeneralizedNewtypeDeriving
newtype T3 a = T3 (D6 a)
  deriving Eq

-- GeneralizedNewtypeDeriving
-- DeriveFunctor
newtype T4 a = T4 (D6 a)
  deriving Functor

-- GeneralizedNewtypeDeriving
-- DeriveDataTypeable
-- DeriveFunctor
-- DeriveFoldable
-- DeriveTraversable
-- DeriveAnyClass
newtype T5 a = T5 (D6 a)
  deriving (Eq, Ord, Ix, Bounded, Show, Read,
            Data, Typeable, Functor, Foldable, Traversable)
