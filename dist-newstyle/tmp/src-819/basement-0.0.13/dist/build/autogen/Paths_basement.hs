{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_basement (
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
version = Version [0,0,13] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/mikpol/.cabal/store/ghc-8.6.5/basement-0.0.13-ba2c7ddbe35cb3e825e6c2767b901085e9795689202e1e2fda9980706d567bfa/bin"
libdir     = "/home/mikpol/.cabal/store/ghc-8.6.5/basement-0.0.13-ba2c7ddbe35cb3e825e6c2767b901085e9795689202e1e2fda9980706d567bfa/lib"
dynlibdir  = "/home/mikpol/.cabal/store/ghc-8.6.5/basement-0.0.13-ba2c7ddbe35cb3e825e6c2767b901085e9795689202e1e2fda9980706d567bfa/lib"
datadir    = "/home/mikpol/.cabal/store/ghc-8.6.5/basement-0.0.13-ba2c7ddbe35cb3e825e6c2767b901085e9795689202e1e2fda9980706d567bfa/share"
libexecdir = "/home/mikpol/.cabal/store/ghc-8.6.5/basement-0.0.13-ba2c7ddbe35cb3e825e6c2767b901085e9795689202e1e2fda9980706d567bfa/libexec"
sysconfdir = "/home/mikpol/.cabal/store/ghc-8.6.5/basement-0.0.13-ba2c7ddbe35cb3e825e6c2767b901085e9795689202e1e2fda9980706d567bfa/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "basement_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "basement_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "basement_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "basement_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "basement_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "basement_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
