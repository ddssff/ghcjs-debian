-- | Was Appraisal.Utils.Files in the image-cache package.
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module ReplaceFile
    ( replaceFile
    ) where

import Control.Exception as E (catch)
import Data.ListLike as LL ()
import Data.ListLike.IO as LL (hPutStr, ListLikeIO, writeFile)
import Prelude hiding (writeFile)
import System.Directory (removeFile)
import System.IO as IO (Handle, IOMode(WriteMode), openFile)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files (getSymbolicLinkStatus, getFdStatus, fileMode, setFdMode)
import System.Posix.IO (handleToFd, closeFd)
import System.Posix.Types (FileMode)

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
