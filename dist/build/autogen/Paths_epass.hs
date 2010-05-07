module Paths_epass (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/baldo/.cabal/bin"
libdir     = "/home/baldo/.cabal/lib/epass-0.0/ghc-6.12.1"
datadir    = "/home/baldo/.cabal/share/epass-0.0"
libexecdir = "/home/baldo/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "epass_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "epass_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "epass_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "epass_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
