{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Ultimate_Tic_Tac_Toe (
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

bindir     = "/home/arnav/.cabal/bin"
libdir     = "/home/arnav/.cabal/lib/x86_64-linux-ghc-8.0.2/Ultimate-Tic-Tac-Toe-0.1.0.0-885gNubF0bkKJCuwYa7CHI"
dynlibdir  = "/home/arnav/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/arnav/.cabal/share/x86_64-linux-ghc-8.0.2/Ultimate-Tic-Tac-Toe-0.1.0.0"
libexecdir = "/home/arnav/.cabal/libexec"
sysconfdir = "/home/arnav/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Ultimate_Tic_Tac_Toe_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Ultimate_Tic_Tac_Toe_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Ultimate_Tic_Tac_Toe_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Ultimate_Tic_Tac_Toe_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Ultimate_Tic_Tac_Toe_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Ultimate_Tic_Tac_Toe_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
