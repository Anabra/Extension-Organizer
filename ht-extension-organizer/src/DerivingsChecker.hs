{-# LANGUAGE FlexibleContexts,
             TypeFamilies,
             TypeSynonymInstances,
             FlexibleInstances
             #-}

{-
  NOTE:
  - DeriveX (X = Data, Generic, Functor, Foldable, Traversable, Lift)
  - same for StandaloneDeriving, but has to check for newtype
  - in case of newtype, we can't really say anything (GeneralizedNtDeriving)
  - DeriveAnyClass (so has to check the "originality" of the class)

  CASES:

  data declaration:
  - is DerivableDefault and original:
    - should do nothing
    - see for details: https://www.haskell.org/onlinereport/derived.html
  - is DerivableExtra and original:
    - add corresponding extension (may still have restrictions)
  - anything else
    - DeriveAnyClass

  newtype declaration:
  - we don't know what the user intended
  - automatically add GeneralizedNewtypeDeriving (it is possible he wanted this)
    - should suggest turning this on (not for Show and Read)
  - however still needs DeriveX for Data and Typeable (and not GNTD)
  - can't generalize nested types
    - ie. they have to be of the form: newtype T v1...vn = T' (t vk+1...vn) deriving (c1...cm)

  StandaloneDeriving:
  - have to lookup type constructor (could be difficult ...)


  TODO:
  - write tests for GADTs, data instances and stand alone derivings
-}


module DerivingsChecker where

import ExtMonad as Ext
import Control.Reference ((.-), (^.), (!~), (&), biplateRef, Traversal)
import Language.Haskell.Tools.Refactor as Refact hiding (Enum)
import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.AST

import Data.Maybe (listToMaybe, fromMaybe)
import Control.Monad.Trans.Maybe

import SrcLoc (RealSrcSpan, SrcSpan(..))
import qualified Name as GHC (Name)
import qualified GHC
import PrelNames

import Debug.Trace


data DerivableClass = ClsEq
                    | ClsOrd
                    | ClsIx
                    | ClsShow
                    | ClsRead
                    | ClsEnum
                    | ClsBounded
                    | ClsData
                    | ClsTypeable
                    | ClsGeneric
                    | ClsFunctor
                    | ClsFoldable
                    | ClsTraversable
  deriving (Show, Read, Eq, Ord)

whichExtension :: DerivableClass -> Maybe Extension
whichExtension ClsEq          = Nothing
whichExtension ClsOrd         = Nothing
whichExtension ClsIx          = Nothing
whichExtension ClsShow        = Nothing
whichExtension ClsRead        = Nothing
whichExtension ClsEnum        = Nothing
whichExtension ClsBounded     = Nothing
whichExtension ClsData        = Just DeriveDataTypeable
whichExtension ClsTypeable    = Just DeriveDataTypeable
whichExtension ClsGeneric     = Just DeriveGeneric
whichExtension ClsFunctor     = Just DeriveFunctor
whichExtension ClsFoldable    = Just DeriveFoldable
whichExtension ClsTraversable = Just DeriveTraversable

readClass :: String -> Maybe DerivableClass
readClass = fmap fst . listToMaybe . filter (null . snd) . reads . ("Cls" ++)

readIntoExt :: String -> Maybe Extension
readIntoExt s = readClass s >>= whichExtension

wiredInClasses = [ eqClassName
                 , ordClassName
                 , ixClassName
                 , showClassName
                 , readClassName
                 , enumClassName
                 , boundedClassName
                 , dataClassName
                 , typeableClassName
                 , genClassName
                 , functorClassName
                 , foldableClassName
                 , traversableClassName
                 ]

isWiredInClass = flip elem wiredInClasses


chkDerivings :: HasNameInfo dom => Decl dom -> ExtMonad dom (Decl dom)
chkDerivings = chkDataDecl
           >=> chkGADTDataDecl
           >=> chkDataInstance
           >=> chkStandaloneDeriving



chkDataDecl :: HasNameInfo dom => Decl dom -> ExtMonad dom (Decl dom)
chkDataDecl d@(DataDecl keyw _ _ _ derivs) = do
  separateByKeyword keyw derivs
  return d
chkDataDecl d = return d

chkGADTDataDecl :: HasNameInfo dom => Decl dom -> ExtMonad dom (Decl dom)
chkGADTDataDecl d@(GADTDataDecl keyw _ _ _ _ derivs) = do
  addOccurenceM GADTs d
  separateByKeyword keyw derivs
  return d
chkGADTDataDecl d = return d

chkDataInstance :: HasNameInfo dom => Decl dom -> ExtMonad dom (Decl dom)
chkDataInstance d@(DataInstance keyw _ _ derivs) = do
  addOccurenceM TypeFamilies d
  separateByKeyword keyw derivs
  return d
chkDataInstance d = return d

separateByKeyword :: HasNameInfo dom =>
                     DataOrNewtypeKeyword dom ->
                     AnnMaybeG UDeriving dom SrcTemplateStage ->
                     ExtMonad dom ()
separateByKeyword keyw derivs
  | isNewtypeDecl keyw = checkWith chkClassForNewtype
  | otherwise          = checkWith chkClassForData
  where checkWith f = do
          (annJust !~ chkDerivingClause f) derivs
          return ()
        isNewtypeDecl keyw = case keyw ^. element of
                               UNewtypeKeyword -> True
                               _               -> False



-- TODO: what if it is a synonym for a newtype?
chkStandaloneDeriving :: HasNameInfo dom => Decl dom -> ExtMonad dom (Decl dom)
chkStandaloneDeriving d@(Refact.StandaloneDeriving instRule) = do
  addOccurenceM Ext.StandaloneDeriving d
  let ihead = instRule ^. irHead
      ty    = rightmostType ihead
      cls   = getClassCon   ihead
  itIsNewType <- isNewtype ty
  if itIsNewType
    then chkClassForNewtype cls
    else chkClassForData    cls
  return d
chkStandaloneDeriving d = return d

getClassCon :: InstanceHead dom -> InstanceHead dom
getClassCon (AppInstanceHead f _) = getClassCon f
getClassCon (ParenInstanceHead x) = getClassCon x
getClassCon x = x

rightmostType :: InstanceHead dom -> Type dom
rightmostType ihead
  | AppInstanceHead _ tyvar <- skipParens ihead = tyvar

-- TODO: Return false if the type is certainly not a newtype
--       Returns true if it is a newtype or it could not have been looked up
isNewtype :: HasNameInfo dom => Type dom -> ExtMonad dom Bool
isNewtype t = do
  result <- runMaybeT . isNewtype' $ t
  return $! fromMaybe True result

isNewtype' :: HasNameInfo dom => Type dom -> MaybeT (ExtMonad dom) Bool
isNewtype' t = do
  name  <- liftMaybe . nameFromType   $ t
  sname <- liftMaybe . getSemName     $ name
  tycon <- MaybeT    . GHC.lookupName $ sname
  return $! isNewtypeTyCon tycon
    where liftMaybe = MaybeT . return

-- NOTE: gives just name if the type being scrutinised can be newtype
--       else it gives nothing
nameFromType :: Type dom -> Maybe (Name dom)
nameFromType (TypeApp f _)    = nameFromType f
nameFromType (ParenType x)    = nameFromType x
nameFromType (KindedType t k) = nameFromType t
nameFromType (VarType x)      = Just x
nameFromType _                = Nothing

isNewtypeTyCon :: GHC.TyThing -> Bool
isNewtypeTyCon (GHC.ATyCon tycon) = GHC.isNewTyCon tycon
isNewtypeTyCon _ = False




chkDerivingClause :: HasNameInfo dom =>
                     (InstanceHead dom -> ExtMonad dom (InstanceHead dom)) ->
                     Deriving dom ->
                     ExtMonad dom (Deriving dom)
chkDerivingClause checker d@(DerivingOne   x)  = checker x >> return d
chkDerivingClause checker d@(DerivingMulti xs) = do
  let classes = xs ^. annListElems
  mapM_ checker classes
  return d

{-
 Checks an individual class inside a deriving clause in a data declaration
 NOTE: If a class in a deriving clause is wired in, and the code compiles,
       means that the class is a DerivableClass.
       If it is not wired in, or it is not a simple class ctor application,
       then it cannot be derived traditionally.

 NOTE: Works only for "class names". If it gets an input from a standalone
       deriving clause, it has to be simplified.
-}
chkClassForData :: HasNameInfo dom =>
                   InstanceHead dom -> ExtMonad dom (InstanceHead dom)
chkClassForData x
  | InstanceHead name <- skipParens x,
    Just sname <- getSemName name,
    isWiredInClass sname
    = do
      let className = name ^. (simpleName & unqualifiedName & simpleNameStr)
      case readIntoExt className of
        Just ext -> addOccurenceM ext x >> return x
        Nothing  -> return x
  | otherwise = addOccurenceM DeriveAnyClass x >> return x


-- TODO: really similar to chkClassForData, try to refactor
-- NOTE: always adds GeneralizedNewtypeDeriving
chkClassForNewtype :: HasNameInfo dom =>
                      InstanceHead dom -> ExtMonad dom (InstanceHead dom)
chkClassForNewtype x
  | InstanceHead name <- skipParens x,
    Just sname <- getSemName name,
    isWiredInClass sname
    = do
      let className = name ^. (simpleName & unqualifiedName & simpleNameStr)
      when (canBeGeneralized sname)
        (addOccurenceM GeneralizedNewtypeDeriving x)
      case readIntoExt className of
        Just ext -> addOccurenceM ext x >> return x
        Nothing  -> return x
  | otherwise = do
      addOccurenceM GeneralizedNewtypeDeriving x
      addOccurenceM DeriveAnyClass x
      return x

canBeGeneralized :: GHC.Name -> Bool
canBeGeneralized = not . flip elem notGNTD
notGNTD = [ dataClassName
          , typeableClassName
          , showClassName
          , readClassName]

skipParens :: InstanceHead dom -> InstanceHead dom
skipParens (ParenInstanceHead x) = skipParens x
skipParens x = x

getSemName :: HasNameInfo dom => Name dom -> Maybe GHC.Name
getSemName x = semanticsName (x ^. simpleName)
