-- | Install (binary) tasks:
--     write debian/ghcjs.substvars
--     write debian/ghcjs.install
--     Fix the wrapper paths in usr/lib/ghcjs/.cabal/bin:
--        ghcjs, ghcjs-boot, ghcjs-run, haddock-ghcjs, hsc2hs-ghcjs
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Exception (try)
import Control.Monad.Trans (MonadIO)
import Data.Char (toLower)
import Data.List as List (intercalate, isPrefixOf, lines)
import Data.Monoid ((<>))
import Data.Text as Text (Text, pack, unpack, unlines, lines)
import Shelly
import System.Environment (setEnv)
import System.FilePath (takeDirectory)
import System.Process (readProcess)
import Text.Regex.TDFA
import ReplaceFile (replaceFile)
import System.Directory (getDirectoryContents)
import System.Environment (getEnv)
import System.Posix.Files (readSymbolicLink)

-- I've never seen this either.  I can only guess what it does.
default (Text)

home :: String
home = "usr/lib/ghcjs"

ab :: String -> String
ab p = "/" <> p

main :: IO ()
main = do
  here <- getEnv "PWD"
  setEnv "HOME" (here <> "/" <> home)
  shelly $ do
    -- Build the debhelper install file for the ghcjs package
    silently (run "find" [pack home, "-type", "f"]) >>=
      liftIO . writeFile "debian/ghcjs.install" . unpack . Text.unlines . map (\ s -> s <> " " <> (pack . takeDirectory . unpack $ s)) . Text.lines
    -- Build the debhelper substvar assignment for the provided libraries
    compilerProvides

  -- Edit the wrapper scripts to reflect the real install directory
  moveWrappers (here <> "/" <> home) ("/" <> home)

-- | Use ghcjs-pkg to find the list of libraries built into ghcjs,
-- turn them into debian virtual package names, and build an
-- assignment to shell variable haskell:Provides.  That goes into the
-- ghcjs.substvars file.
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
moveWrappers :: String -> String -> IO ()
moveWrappers old new =
    getDirectoryContents dir >>= mapM_ moveWrapper
    where
      dir = "usr/lib/ghcjs/.cabal/bin"
      moveWrapper :: String -> IO ()
      moveWrapper "ghcjs-boot" = return () -- not a wrapper
      moveWrapper "ghcjs-run" = return () -- not a wrapper
      moveWrapper linkname =
          (try . readSymbolicLink) (dir <> "/" <> linkname) >>=
          either (\(_ :: IOError) -> return ()) (\scriptname -> editFile fixPaths (dir <> "/" <> scriptname))
      editFile :: (String -> String) -> String -> IO ()
      editFile f p = readFile p >>= replaceFile p . f

      -- Remove /home/dsf/git/ghcjs-debian
      fixPaths :: String -> String
      fixPaths [] = []
      fixPaths s | isPrefixOf old s = new <> fixPaths (drop (length old) s)
      fixPaths (c:s) = c : fixPaths s
