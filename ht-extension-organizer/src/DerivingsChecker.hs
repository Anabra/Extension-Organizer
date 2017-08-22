{-# LANGUAGE FlexibleContexts,
             TypeFamilies,
             TypeSynonymInstances,
             FlexibleInstances,
             MultiWayIf
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
  - lookup tycon at standalone deriving
-}


module DerivingsChecker where

import ExtMonad as Ext
import Control.Reference ((.-), (^.), (!~), (&), biplateRef, Traversal)
import Language.Haskell.Tools.Refactor as Refact hiding (Enum)
import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.AST

import Data.Maybe (listToMaybe)

import SrcLoc (RealSrcSpan, SrcSpan(..))
import qualified Name as GHC (Name)
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

chkStandaloneDeriving :: HasNameInfo dom => Decl dom -> ExtMonad dom (Decl dom)
chkStandaloneDeriving d@(Refact.StandaloneDeriving instRule) = do
  addOccurenceM Ext.StandaloneDeriving d
  return d
chkStandaloneDeriving d = return d

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



chkDerivingClause :: HasNameInfo dom =>
                     (InstanceHead dom -> ExtMonad dom (InstanceHead dom)) ->
                     Deriving dom ->
                     ExtMonad dom (Deriving dom)
chkDerivingClause checker d@(DerivingOne   x)  = checker x >> return d
chkDerivingClause checker d@(DerivingMulti xs) = do
  let classes = xs ^. annListElems
  mapM_ checker classes
  return d

-- Checks an individual class inside a deriving clause in a data declaration
-- NOTE: If a class in a deriving clause is wired in, and the code compiles,
--       means that the class is a DerivableClass.
--       If it is not wired in, or it is not a simple class ctor application,
--       then it cannot be derived traditionally.
chkClassForData :: HasNameInfo dom =>
                   InstanceHead dom -> ExtMonad dom (InstanceHead dom)
chkClassForData x
  | InstanceHead name <- skipParens x,
    Just sname <- getSemName name,
    isWiredInClass sname
    = do
      let className = (name ^. (simpleName & unqualifiedName & simpleNameStr))
      case readIntoExt className of
        Just ext -> addOccurenceM ext x >> return x
        Nothing  -> return x
  | otherwise = addOccurenceM DeriveAnyClass x >> return x


-- TODO: really similar to chkClassForData, try to refactor
chkClassForNewtype :: HasNameInfo dom =>
                      InstanceHead dom -> ExtMonad dom (InstanceHead dom)
chkClassForNewtype x
  | InstanceHead name <- skipParens x,
    Just sname <- getSemName name,
    isWiredInClass sname
    = do
      let className = (name ^. (simpleName & unqualifiedName & simpleNameStr))
      when (canBeGeneralized sname)
        (addOccurenceM GeneralizedNewtypeDeriving x)
      case readIntoExt className of
        Just ext -> addOccurenceM ext x >> return x
        Nothing  -> return x
  | otherwise = do
      addOccurenceM GeneralizedNewtypeDeriving x
      addOccurenceM DeriveAnyClass x
      return x

canBeGeneralized = not . flip elem notGNTD
notGNTD = [ dataClassName
          , typeableClassName
          , showClassName
          , readClassName]

skipParens :: InstanceHead dom -> InstanceHead dom
skipParens (ParenInstanceHead x) = skipParens x
skipParens x = x

getSemName :: HasNameInfo dom => Name dom -> Maybe GHC.Name
getSemName x = semanticsName $ (x ^. simpleName)
