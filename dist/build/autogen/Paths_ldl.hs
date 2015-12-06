module Paths_ldl (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/james/.cabal/bin"
libdir     = "/Users/james/.cabal/lib/i386-osx-ghc-7.4.2/ldl-0.1"
datadir    = "/Users/james/.cabal/share/i386-osx-ghc-7.4.2/ldl-0.1"
libexecdir = "/Users/james/.cabal/libexec"
sysconfdir = "/Users/james/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ldl_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ldl_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ldl_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ldl_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ldl_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
