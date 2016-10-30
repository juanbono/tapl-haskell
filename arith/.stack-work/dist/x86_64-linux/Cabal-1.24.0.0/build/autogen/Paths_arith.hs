{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_arith (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/juan/Programacion/TAPL/Haskell/arith/.stack-work/install/x86_64-linux/lts-7.5/8.0.1/bin"
libdir     = "/home/juan/Programacion/TAPL/Haskell/arith/.stack-work/install/x86_64-linux/lts-7.5/8.0.1/lib/x86_64-linux-ghc-8.0.1/arith-0.1.0.0-8yZ0MMWT8A92EsFJCef6G0"
datadir    = "/home/juan/Programacion/TAPL/Haskell/arith/.stack-work/install/x86_64-linux/lts-7.5/8.0.1/share/x86_64-linux-ghc-8.0.1/arith-0.1.0.0"
libexecdir = "/home/juan/Programacion/TAPL/Haskell/arith/.stack-work/install/x86_64-linux/lts-7.5/8.0.1/libexec"
sysconfdir = "/home/juan/Programacion/TAPL/Haskell/arith/.stack-work/install/x86_64-linux/lts-7.5/8.0.1/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "arith_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "arith_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "arith_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "arith_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "arith_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
