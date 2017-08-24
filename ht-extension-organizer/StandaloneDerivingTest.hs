{-# LANGUAGE DeriveDataTypeable,
             DeriveFunctor,
             DeriveFoldable,
             DeriveTraversable,
             DeriveAnyClass,
             GeneralizedNewtypeDeriving,
             StandaloneDeriving
             #-}

module StandaloneDerivingTest where

import Data.Data
import Data.Typeable
import Data.Generics
import Data.Ix

class C1 a where
  f1 :: a -> ()
  f1 _ = ()



data D0 a = D0

-- OK
deriving instance Show    (D0 a)
deriving instance Read    (D0 a)
deriving instance Eq      (D0 a)
deriving instance Enum    (D0 a)
deriving instance Ord     (D0 a)
deriving instance Bounded (D0 a)
deriving instance Ix      (D0 a)

-- DeriveDataTypeable
-- DeriveFunctor
-- DeriveFoldable
-- DeriveTraversable
deriving instance Data a     => Data     (D0 a)
deriving instance Typeable a => Typeable (D0 a)
deriving instance Functor     D0
deriving instance Foldable    D0
deriving instance Traversable D0

-- DeriveAnyClass
deriving instance C1 (D0 a)


newtype T0 a = T0 (D0 a)

-- OK
deriving instance Show    (T0 a)
deriving instance Read    (T0 a)

-- GeneralizedNewtypeDeriving
deriving instance Eq      (T0 a)
deriving instance Ord     (T0 a)
deriving instance Bounded (T0 a)
deriving instance Ix      (T0 a)

-- DeriveDataTypeable
deriving instance Data a     => Data     (T0 a)
deriving instance Typeable a => Typeable (T0 a)

-- GeneralizedNewtypeDeriving
-- DeriveFunctor
-- DeriveFoldable
-- DeriveTraversable
deriving instance Functor     T0
deriving instance Foldable    T0
deriving instance Traversable T0

-- GeneralizedNewtypeDeriving
-- DeriveAnyClass
deriving instance C1 (T0 a)
