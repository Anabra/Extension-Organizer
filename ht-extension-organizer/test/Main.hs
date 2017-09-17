module Main where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit

import SrcLoc
import GHC hiding (loadModule, ModuleName)
import GHC.Paths (libdir)
import GHC.LanguageExtensions.Type

import Data.List (sort)
import qualified Data.Map.Strict as SMap
import System.FilePath
import System.Directory (listDirectory)

import Language.Haskell.Tools.Refactor hiding (ModuleName)
import Language.Haskell.Tools.PrettyPrint hiding (ModuleName)

import AnnotationParser
import ExtensionOrganizer

import Debug.Trace

main :: IO ()
main = defaultMain $
        testGroup "All tests"
          [ mkTests recordWildCardsTest
          , mkTests flexibleInstancesTest
          , mkTests derivingsTest
          ]

testRoot = "test"

mkModulePath :: FilePath -> ModuleName -> FilePath
mkModulePath testDir testName = testRoot </> testDir </> testName

type TestSuite = (FilePath, [TestName])
type TestName = String
type ModuleName = String
type Line = Int
type SimpleMap = SMap.Map (LogicalRelation Extension) [Line]

spanToLine :: SrcSpan -> Line
spanToLine (RealSrcSpan s) = srcSpanEndLine s

simplifyExtMap :: ExtMap -> SimpleMap
simplifyExtMap = SMap.map (map spanToLine)

loadModuleAST :: FilePath -> ModuleName -> Ghc TypedModule
loadModuleAST dir moduleName = do
  useFlags ["-w"]
  modSummary <- loadModule (testRoot </> dir) moduleName
  parseTyped (mkModulePath dir moduleName) modSummary

getExtensionsFrom :: FilePath -> ModuleName -> IO SimpleMap
getExtensionsFrom dir moduleName = runGhc (Just libdir) $ do
  modAST <- loadModuleAST dir moduleName
  exts <- collectExtensions modAST
  return $! simplifyExtMap exts

getExtAnnotsFrom :: FilePath -> ModuleName -> IO SimpleMap
getExtAnnotsFrom dir moduleName = do
  s <- readFile $ addExtension (mkModulePath dir moduleName) ".hs"
  return $! getExtensionAnnotations s


mkTest :: FilePath -> ModuleName -> TestTree
mkTest dir moduleName = testCase moduleName $ mkAssertion dir moduleName

mkAssertion :: FilePath -> ModuleName -> IO ()
mkAssertion dir moduleName = do
  result   <- getExtensionsFrom dir moduleName
  expected <- getExtAnnotsFrom  dir moduleName
  assertEqual "Failure" (mapSort expected) (mapSort result)
  where mapSort = SMap.map sort

mkTests :: TestSuite -> TestTree
mkTests (testDir, tests) = testGroup testDir (map (mkTest testDir) tests)


recordWildCardsTest = (recordWildCardsRoot, recordWildCardsModules)
recordWildCardsRoot = "RecordWildCardsTest"
recordWildCardsModules = ["Simple"]

flexibleInstancesTest = (flexibleInstancesRoot, flexibleInstancesModules)
flexibleInstancesRoot = "FlexibleInstancesTest"
flexibleInstancesModules = [ "Combined"
                           , "NestedTypes"
                           , "NestedUnitTyCon"
                           , "NestedWiredInType"
                           , "NoFlexInst"
                           , "SameTyVars"
                           , "TopLevelTyVar"
                           , "TopLevelWiredInType"
                           ]

derivingsTest = (derivingsRoot, derivingsModules)
derivingsRoot = "DerivingsTest"
derivingsModules = [ "DataDeriving"
                   , "NewtypeDeriving"
                   , "StandaloneData"
                   , "StandaloneDataSynonyms"
                   , "StandaloneNewtype"
                   , "StandaloneNewtypeAny"
                   , "StandaloneNewtypeSynonyms"
                   , "StandaloneNewtypeSynonymsAny"
                   ]
