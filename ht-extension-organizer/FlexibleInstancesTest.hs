{-# LANGUAGE MultiParamTypeClasses,
             TypeOperators,
             FlexibleInstances,
             KindSignatures
             #-}

module FlexibleInstancesTest where

class C1 a where
  f1 :: a -> Bool

class C2 a b where
  f2 :: a -> b -> Bool

class a :?: b where
  h :: a -> b -> Bool

class a :!: b where
  j :: a -> b -> Bool


data T4 a b c d = T4 a b c d
data T3 a b c = T3 a b c
data T2 a b = T2 a b
data T1 a = T1 a
data T0 = T0

data a :+: b = Plus a b
data (a :++: b) c = PPlus a b c
data a :-: b = Minus a b

-- OK
instance (C1 (((T4) (a)) b c d)) where
    f1 _ = True

-- OK
instance C1 (T2 a b) where
    f1 _ = True

-- OK
instance C1 (T1 a) where
    f1 _ = True

-- OK (because T0 is a type ctor here)
instance C1 T0 where
    f1 _ = True

-- OK
instance C1 (a :+: b) where
  f1 _ = True

-- OK (Doesn't recognize TypeOperators, but in this case, it isn't even needed)
instance C1 ((:-:) a b) where
  f1 _ = True

-- OK
instance (:?:) T0 T0 where
  h _ _ = True

-- OK
instance T0 :!: T0 where
  j _ _ = True

-- OK (MultiParam)
instance T0 :!: (T1 a) where
  j _ _ = True

-- OK (MultiParam)
instance (T2 a b) :!: (T1 a) where
  j _ _ = True

-- OK (MultiParam)
instance (a :+: b) :!: (T1 a) where
  j _ _ = True

-- OK (KindSignatures)
instance C1 [(a :: *)] where
  f1 _ = True

instance C2 (T1 a) (T1 a) where
  f2 _ _ = True

-- FlexibleInstances
instance C1 Int where
  f1 _ = True

  -- FlexibleInstances
instance C1 (T3 a b T0) where
  f1 _ = True

-- FlexibleInstances
instance C1 (T3 a T0 c) where
  f1 _ = True

  -- FlexibleInstances
instance C1 (T3 T0 b c) where
  f1 _ = True

  -- FlexibleInstances
instance C1 (T3 a b Int) where
  f1 _ = True

  -- FlexibleInstances
instance C1 (T3 a Int c) where
  f1 _ = True

  -- FlexibleInstances
instance C1 (T3 Int b c) where
  f1 _ = True

-- FlexibleInstances
instance C2 a a where
  f2 _ _ = True

-- FlexibleInstances
instance C2 ((a :++: b) c) (d) where
  f2 _ _ = True

-- FlexibleInstances
instance C2 a (T2 c c) where
  f2 _ _ = True

-- FlexibleInstances
instance C1 (T1 (T1 a)) where
  f1 _ = True

-- FlexibleInstances
instance C1 (T2 a (T1 b)) where
  f1 _ = True

-- FlexibleInstances
instance C1 (T2 (T1 a) b) where
  f1 _ = True

-- FlexibleInstances
instance C1 ((T1 a) :+: b) where
  f1 _ = True

-- FlexibleInstances
instance C1 (T2 a a) where
  f1 _ = True

-- FlexibleInstances
instance C1 (T4 a b d d) where
  f1 _ = True
