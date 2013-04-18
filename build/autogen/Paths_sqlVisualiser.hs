module Paths_sqlVisualiser (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/akanksha/.cabal/bin"
libdir     = "/home/akanksha/.cabal/lib/sqlVisualiser-0.1.0.0/ghc-7.4.1"
datadir    = "/home/akanksha/.cabal/share/sqlVisualiser-0.1.0.0"
libexecdir = "/home/akanksha/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "sqlVisualiser_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "sqlVisualiser_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "sqlVisualiser_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sqlVisualiser_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
