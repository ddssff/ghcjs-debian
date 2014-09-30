{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Monad.Trans (MonadIO)
import Data.Char (toLower)
import Data.List as List (intercalate, lines)
import Data.Monoid ((<>))
import Data.Text as Text (Text, pack, unpack, unlines, lines)
import Shelly
import System.Environment (setEnv)
import System.FilePath (takeDirectory)
import System.Process (readProcess)
import Text.Regex.TDFA

default (Text)

prefix :: String
prefix = "usr/lib"
home :: String
home = prefix <> "/ghcjs"

ab :: String -> String
ab p = "/" <> p

main :: IO ()
main = do
  setEnv "HOME" (ab home)
  shelly $ do
    -- Build the debhelper install file for the ghcjs package
    silently (run "find" [pack home, "-type", "f"]) >>=
      liftIO . writeFile "debian/ghcjs.install" . unpack . Text.unlines . map (\ s -> s <> " " <> (pack . takeDirectory . unpack $ s)) . Text.lines
    -- Build the debhelper substvar assignment for the provided libraries
    compilerProvides

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
