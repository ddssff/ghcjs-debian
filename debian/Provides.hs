-- | Install (binary) tasks:
--     write debian/ghcjs.substvars
--     write debian/ghcjs.install
--     Fix the wrapper paths in usr/lib/ghcjs/.cabal/bin:
--        ghcjs, ghcjs-boot, ghcjs-run, haddock-ghcjs, hsc2hs-ghcjs
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Exception as E (catch, try)
import Control.Monad (when)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Char (toLower)
import Data.List as List (intercalate, isPrefixOf, lines)
import Data.ListLike as LL ()
import Data.ListLike.IO as LL (hPutStr, ListLikeIO, writeFile)
import Data.Monoid ((<>))
import Data.Text as Text (Text)
import Debug.Trace (trace)
import Prelude hiding (writeFile)
import System.Directory (getDirectoryContents, removeFile)
import System.Environment (getArgs, setEnv)
import System.FilePath (takeDirectory)
import System.Process (readProcess)
import System.Environment (getEnv)
import System.Info (arch, os)
import System.IO as IO (Handle, IOMode(WriteMode), openFile)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files (getSymbolicLinkStatus, getFdStatus, fileMode, readSymbolicLink, setFdMode)
import System.Posix.IO (handleToFd, closeFd)
import System.Posix.Types (FileMode)
import Text.Regex.TDFA

-- I've never seen this either.  I can only guess what it does.
default (Text)

main :: IO ()
main = do
  [top, homeRelative] <- getArgs
  let home = "/" <> homeRelative  -- where the build will eventually end up
      build = top <> homeRelative -- where we will actually create the build

  setEnv "HOME" build

  when (home /= build) (editWrappers home build)

  _ <- readProcess "mkdir" ["-p", "usr/lib"] mempty
  _ <- readProcess "rsync" ["-aHxS", "--delete", (home <> "/"), "usr/lib/ghcjs"] mempty
  findAndRemove homeRelative ["*/config.guess",
                              "*/config.sub",
                              "*/ghcjs-boot",
                              "*/packages/hackage.haskell.org",
                              "*/.cabal/logs",
                              "*/.cabal/lib",
                              "*/.cabal/packages",
                              "*/.cabal/setup-exe-cache",
                              "*/.cabal/share",
                              "*/.ghc",
                              "*/.git"]
  readProcess "find" [homeRelative, "-type", "f"] mempty >>=
    writeFile "debian/ghcjs.install" . unlines . map formatInstallLine . lines

  compilerProvides
  buildTriggers home
  buildLinks home

  when (home /= build) (editWrappers build home)

formatInstallLine :: String -> String
formatInstallLine s = "/" <> s <> " " <> ("/" <> takeDirectory s)

findAndRemove :: FilePath -> [String] -> IO ()
findAndRemove top patterns =
    readProcess "find" (top : intercalate ["-o"] (map (\x -> ["-path", x]) patterns)) mempty >>=
    mapM_ (\x -> readProcess "rm" ["-rf", x] mempty) . lines

modifyEnv :: String -> (String -> String) -> IO ()
modifyEnv var f = getEnv var >>= \old -> setEnv var (f old)


-- | Use ghcjs-pkg to find the list of libraries built into ghcjs,
-- turn them into debian virtual package names, and build an
-- assignment to shell variable haskell:Provides.  That goes into the
-- ghcjs.substvars file.  There may already be something there by the
-- time this is called, so we append.  So, not idempotent.
compilerProvides :: MonadIO m => m ()
compilerProvides = liftIO $ compilerLibs >>= appendFile "debian/ghcjs.substvars" . providesLine

compilerLibs :: IO [String]
compilerLibs = (concatMap parseLib . List.lines) <$> readProcess "ghcjs-pkg" ["list", "-v2"] ""

providesLine :: [String] -> String
providesLine libs = "haskell:Provides=" ++ intercalate ", " libs ++ "\n"

parseLib :: String -> [String]
parseLib s =
    case s =~ ("^.*\\((.*)-([0-9.]*)-(.....)..........*\\)$" :: String) :: (String, String, String, [String]) of
      (_, _, _, [name,ver,chk]) ->
          ["libghcjs-" <> map toLower name <> "-dev",
           "libghcjs-" <> map toLower name <> "-dev-" <> ver <> "-" <> chk]
      _ -> trace ("Could not parse: " ++ show s) []

-- | Strip the prefix containing $PWD from the paths in the wrapper
-- scripts, leaving paths starting with /usr/lib/ghcjs.  Also, built
-- the ghcjs.links file.  This is not used because we can't actually
-- build in a location different from the eventual install, but maybe
-- someday...
editWrappers :: String -> String -> IO ()
editWrappers build homeRelative =
    getDirectoryContents bin >>= mapM_ doFile
    where
      home = "/" <> homeRelative
      bin = home <> "/.cabal/bin"
      doFile linkname =
          (try . readSymbolicLink) (bin <> "/" <> linkname) >>=
          either (\(_ :: IOError) -> return ()) (doLink linkname)
      doLink linkname linktext =
        editWrapper linkname linktext

      editWrapper :: String -> String -> IO ()
      editWrapper "ghcjs-boot" _ = return () -- not a wrapper
      editWrapper "ghcjs-run" _ = return () -- not a wrapper
      editWrapper _ linktext =
          readFile (bin <> "/" <> linktext) >>= replaceFile (bin <> "/" <> linktext) . fixPaths

      -- Remove prefix $HOME from wrapper script paths
      fixPaths :: String -> String
      fixPaths [] = []
      fixPaths s | isPrefixOf home s = home <> fixPaths (drop (length (home)) s)
      fixPaths s | isPrefixOf build s = home <> fixPaths (drop (length build) s)
      fixPaths (c:s) = c : fixPaths s

buildLinks :: String -> IO ()
buildLinks home =
    getDirectoryContents bin >>= mapM_ doFile
    where
      bin = home <> "/.cabal/bin"
      doFile linkname =
          (try . readSymbolicLink) (bin <> "/" <> linkname) >>=
          either (\(_ :: IOError) -> return ()) (doLink linkname)
      doLink linkname linktext = do
        ln (bin <> "/" <> linkname) (bin <> "/" <> linktext)
        ln ("/usr/bin/" <> linkname) (bin <> "/" <> linktext)
      ln linktext linkpath = appendFile "debian/ghcjs.links" (linkpath <> " " <> linktext <> "\n")

buildTriggers :: String -> IO ()
buildTriggers home = do
  ghcjsVersion <- (head . lines) <$> readProcess "ghcjs" ["--numeric-version"] mempty
  ghcjsGhcVersion <- (head . lines) <$> readProcess "ghcjs" ["--numeric-ghc-version"] mempty
  writeFile "debian/ghcjs.triggers" ("interest " <> home <> "/.ghcjs/" <> intercalate "-" [arch, os, ghcjsVersion, ghcjsGhcVersion] <> "/ghcjs/package.conf.d\n")

-- | Replace a file's contents, accounting for the possibility that the
-- old contents of the file may still be being read.
replaceFile :: forall full item. (ListLikeIO full item, Eq full) => FilePath -> full -> IO ()
replaceFile path text =
    (getSymbolicLinkStatus path >>= return . fileMode >>= \mode ->
     removeFile path >> writeFileAndFixMode (const mode) path text)
        `E.catch` (\ e -> if isDoesNotExistError e then writeFile path text else ioError e)

-- | Write a file and make it readable
writeFileAndFixMode :: forall full item. (ListLikeIO full item, Eq full) => (FileMode -> FileMode) -> FilePath -> full -> IO ()
writeFileAndFixMode fixmode path bytes = do
  fp <- IO.openFile path IO.WriteMode
  hPutStr fp bytes
  setModeAndClose fixmode fp

setModeAndClose :: (FileMode -> FileMode) -> IO.Handle -> IO ()
setModeAndClose modefn fp = do
  -- This closes the handle (but not the fd)
  fd <- handleToFd fp
  mode <- fileMode <$> getFdStatus fd
  setFdMode fd (modefn mode)
  closeFd fd
