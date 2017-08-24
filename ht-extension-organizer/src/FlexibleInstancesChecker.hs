{-# LANGUAGE FlexibleContexts,
             TypeFamilies,
             TypeSynonymInstances,
             FlexibleInstances,
             MultiWayIf
             #-}

module FlexibleInstancesChecker where

import ExtMonad
import Control.Reference ((.-), (^.), (!~), (&), biplateRef, Traversal)
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.AST


import Data.List (nub)
import Data.Data
import qualified Data.Map.Strict as SMap
import Control.Monad.Trans.Maybe
import Control.Monad.Identity

import SrcLoc (RealSrcSpan, SrcSpan(..))
import Name as GHC (isTyVarName, isTyConName, isWiredInName)

import Debug.Trace

{-# ANN module "HLint: ignore Redundant bracket" #-}

-- TODO: write "deriving instance ..." tests (should work)
-- TODO: should expand type synonims  !!!

-- TODO: as in PlaceComments
chkFlexibleInstances sp = nodesContained sp !~ chkInstanceRule


-- this check DOES transform the AST for its internal computations
-- but returns the original one at the end
-- NOTE: There are two traversal:
--       First one on the class level, and the second one one on the type level.
--       Since biplateRef is lazy, it won't go down to the type level in the first traversal
chkInstanceRule :: ExtDomain dom =>
                   InstanceRule dom -> ExtMonad dom (InstanceRule dom)
chkInstanceRule r@(InstanceRule _ _ ihead) = do
  chkInstanceHead ihead
  return $! r
chkInstanceRule r = return r

refact ::
     (Data.Data.Data (node dom stage), Data.Data.Data (inner dom stage),
      Monad m) =>
     (inner dom stage -> m (inner dom stage))
     -> node dom stage -> m (node dom stage)
refact op = biplateRef !~ op


-- one IHApp will only check its own tyvars (their structure and uniqueness)
-- thus with MultiParamTypeclasses each param will be checked independently
-- (so the same type variable can appear in multiple params)
chkInstanceHead :: ExtDomain dom =>
                   InstanceHead dom -> ExtMonad dom (InstanceHead dom)
chkInstanceHead x@(InfixInstanceHead tyvars op) = do
  tyvars' <- refact rmTypeMisc tyvars
  chkTyVars tyvars'
  addOccurenceM MultiParamTypeClasses x
  addOccurenceM TypeOperators x
  return x
chkInstanceHead app@(AppInstanceHead f tyvars) = do
  tyvars' <- refact rmTypeMisc tyvars
  chkTyVars tyvars'
  case f of
    AppInstanceHead _ _ -> addOccurenceM MultiParamTypeClasses app
    _ -> return ()
  chkInstanceHead f
  return app
chkInstanceHead x@(ParenInstanceHead h) = do
  chkInstanceHead h
  return x
chkInstanceHead app = return app

-- TODO: skip other unnecessary parts of the AST (eg.: UType ctors)
-- where can UTyPromoted appear?
-- can i write forall in instance heads?
-- unboxed tuple (has different kind, can't use in ihead), par array?
-- TH ctors
-- other misc ...
chkTyVars :: ExtDomain dom => Type dom -> ExtMonad dom (Type dom)
chkTyVars vars = do
  exts <- get
  (isOk, (cs, vs)) <- runStateT (runMaybeT (chkAll vars)) ([],[])
  case isOk of
    Just isOk ->
      unless (isOk && length vs == (length . nub $ vs)) --tyvars are different
        (addOccurenceM FlexibleInstances vars)
    _         -> error "chkTyVars: Couldn't look up something"
  return vars

  where chkAll x =
          ifM (chkTopLevel x) $
            chkOnlyApp x

        chkTopLevel x = -- NOTE: this resembles a monadic bind ... (Cont?)
          ifM (chkListType x) .
            ifM (chkTupleType x) .
              ifM (chkUnitTyCon x) $
                return False

        ifM cond f = do b <- cond; if b then (return b) else f

        chkUnitTyCon (VarType x) = do
          sname <- tyVarSemNameM x
          -- standalone top-level type variables are not accepted
          -- NOTE: -XHaskell98 operator type variables??
          -- NOTE VarType is either TyCon or TyVar
          --      if it is a TyCon, it cannot be wired in (Int, Char, etc)
          if | isTyVarName   sname -> addTyVarM x >> return False
             | isWiredInName sname -> addTyConM x >> return False
             | isTyConName   sname -> addTyConM x >> return True
             | otherwise            -> return True -- NEVER
        chkUnitTyCon _ = return False


        chkSingleTyVar (VarType x) = do
          sname <- tyVarSemNameM x
          if (isTyVarName sname)
            then addTyVarM x >> return True
            else addTyConM x >> return False
        chkSingleTyVar _ = return False


        chkTupleType (TupleType args) = do
          let xs  = args ^. annListElems
          bs <- mapM chkSingleTyVar xs
          return $! and bs
        chkTupleType _ = return False

        chkListType (ListType v) = chkSingleTyVar v
        chkListType _            = return False

        chkOnlyApp :: (MonadState ([Name dom],[Name dom]) (m1 m2),
                       MonadTrans m1,
                       MonadState (SMap.Map Extension [SrcSpan]) m2,
                       ExtDomain dom) =>
                       Type dom -> MaybeT (m1 m2) Bool
        chkOnlyApp (TypeApp f v@(VarType x)) = do
          isTyVar <- chkSingleTyVar v
          if isTyVar
            then case f of
              (VarType c) -> addTyConM c >> return True
              _           -> chkOnlyApp f
            else return False
        chkOnlyApp x@(InfixTypeApp lhs op rhs) = do
          lift . lift $ addOccurenceM TypeOperators x
          addTyConM . mkNormalName $ (op ^. operatorName)
          lOK <- chkSingleTyVar lhs
          rOK <- chkSingleTyVar rhs
          return $! lOK && rOK
        chkOnlyApp _ = return False

        addTyCon  n (ctors, vars) = (n:ctors, vars)
        addTyVar  n (ctors, vars) = (ctors, n:vars)
        addTyConM n               = modify $ addTyCon n
        addTyVarM n               = modify $ addTyVar  n

        tyVarSemNameM x = MaybeT . return . semanticsName $ x ^. simpleName

rmTypeMisc :: Type dom -> ExtMonad dom (Type dom)
rmTypeMisc = rmTParens >=> rmTKinded

rmTKinded :: Type dom -> ExtMonad dom (Type dom)
rmTKinded kt@(KindedType t _) = addOccurenceM KindSignatures kt >> return t
rmTKinded x                   = return x

-- removes Parentheses from the AST
-- the structure is reserved
rmTParens :: Type dom -> ExtMonad dom (Type dom)
rmTParens (ParenType x) = return x
rmTParens x             = return x
