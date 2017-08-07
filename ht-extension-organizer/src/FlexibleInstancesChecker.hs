{-# LANGUAGE FlexibleContexts,
             TypeFamilies,
             TypeSynonymInstances,
             FlexibleInstances,
             MultiWayIf
             #-}

module FlexibleInstancesChecker where

import ExtMonad
import Control.Reference ((.-), (^.), (!~), (&), Traversal)
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.AST


import Data.List (nub)
import Control.Monad.Trans.Maybe

import SrcLoc (RealSrcSpan, SrcSpan(..))
import Name as GHC (isTyVarName, isTyConName, isWiredInName)

import Debug.Trace

refact op arg = let SrcLoc.RealSrcSpan r = getRange arg
                in (nodesContained r !~ op) arg

chkFlexibleInstances sp = (nodesContained sp !~ chkInstanceRule)


-- this check DOES transform the AST for its internal computations
-- but returns the original one at the end
chkInstanceRule :: ExtDomain dom =>
                   InstanceRule dom -> ExtMonad dom (InstanceRule dom)
chkInstanceRule r@(InstanceRule _ ctx ihead) = do
  ihead'  <- refact rmIHParens ihead
  ihead'' <- refact trfIHInfix ihead'
  refact chkInstanceHeadApp ihead''
  return $! r
chkInstanceRule r = return r


-- one IHApp will only check its own tyvars (their structure and uniqueness)
-- thus with MultiParamTypeclasses each param will be checked independently
-- (so the same type variable can appear in multiple params)
chkInstanceHeadApp :: ExtDomain dom =>
                      InstanceHead dom -> ExtMonad dom (InstanceHead dom)
chkInstanceHeadApp app@(AppInstanceHead _ tyvars) = do
  tyvars'  <- refact rmTParens tyvars
  tyvars'' <- refact trfTInfix tyvars'
  chkTyVars tyvars''
  return app
chkInstanceHeadApp app = return app

-- TODO: remove other uncessary parts of the AST (eg.: kind annotaions)
chkTyVars :: ExtDomain dom => Type dom -> ExtMonad dom (Type dom)
chkTyVars vars = do
  -- TODO: check Nothing
  let (Just isOk, (cs, vs)) = runState (runMaybeT (chkAll vars)) ([],[])
  unless (isOk && length vs == (length . nub $ vs)) --tyvars are different
    (addOccurenceM FlexibleInstances vars)
  return vars

  where chkAll x = do
          ifM (chkTopLevel x) $
            isOnlyAppM x

        chkTopLevel x = do -- NOTE: this resembles a monadic bind ... (Cont?)
          ifM (isListType x) .
            ifM (isTupleType x) .
              ifM (isUnitTyCon x) $
                return False

        isUnitTyCon (VarType x) = do
          sname <- tyVarSemNameM x
          -- standalone top-level type variables are not accepted
          -- NOTE: -XHaskell98 operator type variables??
          -- NOTE VarType is either TyCon or TyVar
          --      if it is a TyCon, it cannot be wired in (Int, Char, etc)
          if | isTyVarName   sname -> addTyVarM x >> return False
             | isWiredInName sname -> addTyConM x >> return False
             | isTyConName   sname -> addTyConM x >> return True
             | otherwise            -> return True -- NEVER
        isUnitTyCon _ = return False

        isSingleTyVar (VarType x) = do
          sname <- tyVarSemNameM x
          if (isTyVarName sname) then (return True) else (return False)
        isSingleTyVar _ = return False

        isTupleType (TupleType args) = do
          let xs  = args ^. annListElems
          bs <- mapM isSingleTyVar xs
          return $! and bs
        isTupleType _ = return False

        isListType (ListType v) = isSingleTyVar v
        isListType _            = return False

        -- TODO: separate top-level check
        isOnlyAppM :: (MonadState ([Name dom],[Name dom]) m, ExtDomain dom) =>
                      Type dom -> MaybeT m Bool
        isOnlyAppM (TypeApp f (VarType x)) = do
          sname <- tyVarSemNameM x
          if (isTyVarName sname)
            then do
              addTyVarM x
              case f of
                (VarType c) -> addTyConM c >> return True
                _           -> isOnlyAppM f
            else addTyConM x >> return False
        isOnlyAppM _ = return False

        addTyCon  n (ctors, vars) = (n:ctors, vars)
        addTyVar  n (ctors, vars) = (ctors, n:vars)
        addTyConM n               = modify $ addTyCon n
        addTyVarM n               = modify $ addTyVar  n

        tyVarSemNameM x = MaybeT . return . semanticsName $ x ^. simpleName


-- removes Parentheses from the AST
-- the structure is reserved
rmTParens :: Type dom -> ExtMonad dom (Type dom)
rmTParens (ParenType x) = return x
rmTParens x             = return x

rmIHParens :: InstanceHead dom -> ExtMonad dom (InstanceHead dom)
rmIHParens (ParenInstanceHead x) = return x
rmIHParens x                     = return x

-- transforms infix applications to normal application
-- while maintaining the operator name
-- TODO: make it so that it maintains the RealSrcSpan (location info)
-- NOTE: also checks for TypeOperators
trfTInfix :: Type dom -> ExtMonad dom (Type dom)
trfTInfix x@(InfixTypeApp lhs op rhs) = do
  addOccurenceM TypeOperators x
  return $! mkTypeApp (mkTypeApp op' lhs) rhs
  where op' = mkVarType . mkNormalName $ (op ^. operatorName)
trfTInfix t = return t

trfIHInfix :: InstanceHead dom -> ExtMonad dom (InstanceHead dom)
trfIHInfix x@(InfixInstanceHead lhs op) = do
  addOccurenceM TypeOperators x
  return $! mkAppInstanceHead (mkInstanceHead op') lhs
  where op' = mkNormalName (op ^. operatorName)
trfIHInfix x = return x

getElems :: Traversal (AnnListG e d s) (AnnListG e d s) (e d s) (e d s)
getElems = annList & element

ifM cond f = do b <- cond; if b then (return b) else f

notM cond = do b <- cond; return . not $ b
