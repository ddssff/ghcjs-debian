-- | Install (binary) tasks:
--     write debian/ghcjs.substvars
--     write debian/ghcjs.install
--     Fix the wrapper paths in usr/lib/ghcjs/.cabal/bin:
--        ghcjs, ghcjs-boot, ghcjs-run, haddock-ghcjs, hsc2hs-ghcjs
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Exception as E (catch, try)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Char (toLower)
import Data.List as List (intercalate, isPrefixOf, lines)
import Data.ListLike as LL ()
import Data.ListLike.IO as LL (hPutStr, ListLikeIO, writeFile)
import Data.Monoid ((<>))
import Data.Text as Text (Text)
import Prelude hiding (writeFile)
import System.Directory (getDirectoryContents, removeFile)
import System.Environment (getArgs, setEnv)
import System.FilePath (takeDirectory)
import System.Process (readProcess)
import System.Environment (getEnv)
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
  let homeAbsolute = top <> homeRelative

  build <- (++ homeAbsolute) <$> getEnv "PWD"
  setEnv "HOME" build

  -- For debugging, reverse the edits on the wrappers so they work "in
  -- place".
  editWrappers homeAbsolute build

  -- Make sure the new ghcjs binaries are in the PATH
  modifyEnv "PATH" ((build <> "/.cabal/bin:") <>)

  readProcess "find" [homeRelative, "-type", "f"] mempty >>=
    liftIO . writeFile "debian/ghcjs.install" . unlines . map (\ s -> s <> " " <> (takeDirectory $ s)) . lines

  compilerProvides
  editWrappers build homeAbsolute

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
    case s =~ ("^.*\\((.*)-([0-9.]*)-(.....)...........................\\)$" :: String) :: (String, String, String, [String]) of
      (_, _, _, [name,ver,chk]) ->
          ["libghcjs-" <> map toLower name <> "-dev",
           "libghcjs-" <> map toLower name <> "-dev-" <> ver <> "-" <> chk]
      _ -> []

-- | Strip the prefix containing $PWD from the paths in the wrapper scripts,
-- leaving paths starting with /usr/lib/ghcjs.
editWrappers :: String -> String -> IO ()
editWrappers old new =
    getDirectoryContents dir >>= mapM_ doFile
    where
      dir = "usr/lib/ghcjs/.cabal/bin"
      doFile linkname =
          (try . readSymbolicLink) (dir <> "/" <> linkname) >>=
          either (\(_ :: IOError) -> return ()) (doLink linkname)
      doLink linkname linktext = do
        ln ("/" <> dir <> "/" <> linkname) ("/" <> dir <> "/" <> linktext)
        ln ("/usr/bin/" <> linkname) ("/" <> dir <> "/" <> linktext)
        editWrapper linkname linktext

      ln linktext linkpath = appendFile "debian/ghcjs.links" (linkpath <> " " <> linktext <> "\n")

      editWrapper :: String -> String -> IO ()
      editWrapper "ghcjs-boot" _ = return () -- not a wrapper
      editWrapper "ghcjs-run" _ = return () -- not a wrapper
      editWrapper _ linktext =
          readFile (dir <> "/" <> linktext) >>= replaceFile (dir <> "/" <> linktext) . fixPaths

      -- Remove prefix $HOME from wrapper script paths
      fixPaths :: String -> String
      fixPaths [] = []
      fixPaths s | isPrefixOf new s = new <> fixPaths (drop (length new) s)
      fixPaths s | isPrefixOf old s = new <> fixPaths (drop (length old) s)
      fixPaths (c:s) = c : fixPaths s

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
