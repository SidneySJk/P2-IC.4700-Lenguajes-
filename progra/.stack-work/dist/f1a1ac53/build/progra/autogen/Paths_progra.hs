{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_progra (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\TEC CODIGO\\Lenguajes\\Proyectos\\Proyecto2\\P2-IC.4700-Lenguajes-\\progra\\.stack-work\\install\\5df90267\\bin"
libdir     = "C:\\TEC CODIGO\\Lenguajes\\Proyectos\\Proyecto2\\P2-IC.4700-Lenguajes-\\progra\\.stack-work\\install\\5df90267\\lib\\x86_64-windows-ghc-9.4.8\\progra-0.1.0.0-Dc11GNbfLlVAUXnIleUz4s-progra"
dynlibdir  = "C:\\TEC CODIGO\\Lenguajes\\Proyectos\\Proyecto2\\P2-IC.4700-Lenguajes-\\progra\\.stack-work\\install\\5df90267\\lib\\x86_64-windows-ghc-9.4.8"
datadir    = "C:\\TEC CODIGO\\Lenguajes\\Proyectos\\Proyecto2\\P2-IC.4700-Lenguajes-\\progra\\.stack-work\\install\\5df90267\\share\\x86_64-windows-ghc-9.4.8\\progra-0.1.0.0"
libexecdir = "C:\\TEC CODIGO\\Lenguajes\\Proyectos\\Proyecto2\\P2-IC.4700-Lenguajes-\\progra\\.stack-work\\install\\5df90267\\libexec\\x86_64-windows-ghc-9.4.8\\progra-0.1.0.0"
sysconfdir = "C:\\TEC CODIGO\\Lenguajes\\Proyectos\\Proyecto2\\P2-IC.4700-Lenguajes-\\progra\\.stack-work\\install\\5df90267\\etc"

getBinDir     = catchIO (getEnv "progra_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "progra_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "progra_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "progra_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "progra_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "progra_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
