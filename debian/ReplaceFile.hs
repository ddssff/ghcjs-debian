-- | Was Appraisal.Utils.Files in the image-cache package.
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module ReplaceFile
    ( replaceFile
    ) where

import Control.Exception as E (catch)
import Data.ListLike as LL (hPutStr, ListLikeIO)
import System.Directory (removeFile)
import System.IO as IO (Handle, IOMode(WriteMode), openFile)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files (getFdStatus, fileMode, setFdMode, unionFileModes, ownerReadMode, groupReadMode, otherReadMode)
import System.Posix.IO (handleToFd, closeFd)

-- | Replace a file's contents, accounting for the possibility that the
-- old contents of the file may still be being read.
replaceFile :: forall full item. (ListLikeIO full item, Eq full) => FilePath -> full -> IO ()
replaceFile path text = removeFileIfPresent path >> writeFileReadable path text

-- | Remove a file if it exists, do nothing if it does not exists,
-- rethrow any other exception.
removeFileIfPresent :: FilePath -> IO ()
removeFileIfPresent path = removeFile path `E.catch` (\ e -> if isDoesNotExistError e then return () else ioError e)

-- | Write a file and make it readable
writeFileReadable :: forall full item. (ListLikeIO full item, Eq full) => FilePath -> full -> IO ()
writeFileReadable path bytes = do
  fp <- IO.openFile path IO.WriteMode
  hPutStr fp bytes
  makeReadableAndClose fp

makeReadableAndClose :: IO.Handle -> IO ()
makeReadableAndClose fp = do
  -- This closes the handle (but not the fd)
  fd <- handleToFd fp
  mode <- fileMode <$> getFdStatus fd
  let mode' = foldr unionFileModes mode [ownerReadMode, groupReadMode, otherReadMode]
  setFdMode fd mode'
  closeFd fd
