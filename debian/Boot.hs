{-# LANGUAGE OverloadedStrings #-}

import Data.List (intersperse)
import Data.Monoid ((<>))
import Data.Text as Text (Text, pack, lines)
import Shelly
import System.Environment (setEnv)
import System.IO (hPutStrLn, stderr)

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
    rm_rf (fromText (pack (ab home)))
    mkdir_p (fromText (pack (ab home)) </> pack ".cabal")
    run_ "cabal" ["update"]
    run_ "ghcjs-boot" ["--with-node", "/usr/bin/nodejs"]
    -- Clean out files we don't want in the binary package
    let junk = ["*/config.guess", "*/config.sub", "*/ghcjs-boot", "*/packages/hackage.haskell.org", "*/.cabal/logs", "*/.git"]
    run "find" ([pack (ab home)] ++ concat (intersperse ["-o"] (map (\ p -> ["-path", p]) junk))) >>= mapM_ (rm_rf . fromText) . Text.lines
    -- Copy into the build directory
    mkdir_p (fromText (pack home))
    run_ "rsync" ["-aHxS", "--delete", pack (ab home) <> "/", pack home]
    liftIO $ hPutStrLn stderr "Finished rsync"
