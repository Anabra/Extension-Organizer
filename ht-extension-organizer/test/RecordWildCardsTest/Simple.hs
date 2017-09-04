{-# LANGUAGE RecordWildCards #-}

module Simple where

data T = T { a :: Int
           , b :: Int -> Bool
           , c :: Double
           }

f :: T -> Bool
f T{a=1,c=1,..} = True {-* RecordWildCards *-}
f T{..} = True         {-* RecordWildCards *-}

g :: T
g = let a = 3
        b = const True
        c = 3.14
    in T{..} {-* RecordWildCards *-}
