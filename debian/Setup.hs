{-# LANGUAGE OverloadedStrings #-}
import System.IO (hPutStrLn, stderr)

import Distribution.Simple (defaultMainWithHooks, simpleUserHooks, hookedPrograms, UserHooks(..))
import Distribution.Simple.Program.Types (simpleProgram)
import Distribution.Simple.Setup (CopyDest(..), installVerbosity, copyVerbosity, fromFlag)
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose, installOrdinaryFile)
import Distribution.Simple.LocalBuildInfo (buildDir, absoluteInstallDirs, InstallDirs, LocalBuildInfo(..))
import Distribution.PackageDescription (PackageDescription(..), BuildInfo(..), Executable(..))

import System.Process (rawSystem)
import Control.Monad (when)
import Data.Maybe (maybe, listToMaybe)
import Prelude hiding (FilePath, lines, unlines)
import Data.Char (toLower)
import Data.List (intercalate, intersperse)
import Data.Maybe (mapMaybe)
import Debian.Relation (BinPkgName(..))
import System.Environment (setEnv)
import System.FilePath (takeDirectory)
import Filesystem.Path (dirname)
import Shelly
import Text.Regex.TDFA
import Data.Text (Text, pack, unpack, unlines, lines)
import Data.Monoid ((<>))

default (Text)

prefix :: String
prefix = "usr/lib"
home :: String
home = prefix <> "/ghcjs"

ab :: String -> String
ab p = "/" <> p

main = do
  setEnv "HOME" (ab home)
  shelly $ do
    rm_rf (fromText (pack (ab home)))
    mkdir_p (fromText (pack (ab home)) </> pack ".cabal")
    run_ "cabal" ["update"]
    run_ "ghcjs-boot" ["--with-node", "/usr/bin/nodejs"]
    -- Clean out files we don't want in the binary package
    let junk = ["*/config.guess", "*/config.sub", "*/ghcjs-boot", "*/packages/hackage.haskell.org", "*/.cabal/logs", "*/.git"]
    run "find" ([pack (ab home)] ++ concat (intersperse ["-o"] (map (\ p -> ["-path", p]) junk))) >>= mapM_ (rm_rf . fromText) . lines
    -- Copy into the build directory
    mkdir_p (fromText (pack home))
    run_ "rsync" ["-aHxS", "--delete", pack (ab home) <> "/", pack home]
    -- Build the debhelper install file for the ghcjs package
    silently (run "find" [pack home, "-type", "f"]) >>=
      liftIO . writeFile "debian/ghcjs.install" . unpack . unlines . map (\ s -> s <> " " <> (pack . takeDirectory . unpack $ s)) . lines
    -- Build the debhelper substvar assignment for the provided libraries
    hcProvides

-- | Use ghcjs-pkg to find the list of libraries built into ghcjs,
-- turn them into debian virtual package names, and build an
-- assignment to shell variable haskell:Provides.  That goes into the
-- ghcjs.substvars file.
hcProvides = do
  pkgs <- pwd >>= \ here -> run "ghcjs-pkg" ["list", "-v2"] >>= return . concatMap (parseLib . unpack) . lines
  liftIO $ appendFile "debian/ghcjs.substvars" ("haskell:Provides=" ++ intercalate ", " (map (\ (BinPkgName name) -> name) pkgs) ++ "\n")
    where
      parseLib :: String -> [BinPkgName]
      parseLib s = case s =~ ("^.*\\((.*)-([0-9.]*)-(.....)...........................\\)$" :: String) :: (String, String, String, [String]) of
                     (_, _, _, [name,ver,sum]) ->
                         [BinPkgName ("libghcjs-" <> map toLower name <> "-dev"),
                          BinPkgName ("libghcjs-" <> map toLower name <> "-dev-" <> ver <> "-" <> sum)]
                     _ -> []
