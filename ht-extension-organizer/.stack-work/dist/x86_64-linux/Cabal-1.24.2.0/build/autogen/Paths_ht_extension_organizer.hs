{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_ht_extension_organizer (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/anabra/ELTE/Haskell_Tools/ht-extension-organizer/Extension-Organizer/ht-extension-organizer/.stack-work/install/x86_64-linux/lts-9.3/8.0.2/bin"
libdir     = "/home/anabra/ELTE/Haskell_Tools/ht-extension-organizer/Extension-Organizer/ht-extension-organizer/.stack-work/install/x86_64-linux/lts-9.3/8.0.2/lib/x86_64-linux-ghc-8.0.2/ht-extension-organizer-0.1.0.0-1NgBbe619AH4JExLErEUCI"
dynlibdir  = "/home/anabra/ELTE/Haskell_Tools/ht-extension-organizer/Extension-Organizer/ht-extension-organizer/.stack-work/install/x86_64-linux/lts-9.3/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/anabra/ELTE/Haskell_Tools/ht-extension-organizer/Extension-Organizer/ht-extension-organizer/.stack-work/install/x86_64-linux/lts-9.3/8.0.2/share/x86_64-linux-ghc-8.0.2/ht-extension-organizer-0.1.0.0"
libexecdir = "/home/anabra/ELTE/Haskell_Tools/ht-extension-organizer/Extension-Organizer/ht-extension-organizer/.stack-work/install/x86_64-linux/lts-9.3/8.0.2/libexec"
sysconfdir = "/home/anabra/ELTE/Haskell_Tools/ht-extension-organizer/Extension-Organizer/ht-extension-organizer/.stack-work/install/x86_64-linux/lts-9.3/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ht_extension_organizer_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ht_extension_organizer_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ht_extension_organizer_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ht_extension_organizer_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ht_extension_organizer_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ht_extension_organizer_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
