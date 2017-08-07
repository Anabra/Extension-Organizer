-- why the type variables?

-- | Parts of a qualified name.
data UNamePart dom stage
  = UNamePart { _simpleNameStr :: String }

-- | Program elements formatted as string literals (import packages, pragma texts)
data UStringNode dom stage
  = UStringNode { _stringNodeStr :: String }

MaybeT monad tower
