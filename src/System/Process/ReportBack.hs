{-# LANGUAGE LambdaCase #-}
module System.Process.ReportBack
  ( reportingBack
  , sendReport
  , DecodeFailed(..)
  ) where

import System.FSNotify
import Control.Exception
import System.IO.Temp
import System.Environment
import Control.Concurrent.MVar
import System.Directory
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import System.FilePath
import System.IO (hClose)

-- | User provided decode function failed.
data DecodeFailed = DecodeFailed deriving Show

instance Exception DecodeFailed

-- | Run some action, expecting it to leave some results in the given
-- 'FilePath'.
--
-- An example where this may be useful is if we're starting a process
-- on a dynamic port from the action: we won't know what port is
-- allocated until after the process binds the port. If original
-- process needs this information (to connect, for example), it needs
-- to be told about the result.
--
-- You can think of this as poor-man's one-time IPC.
--
-- You probably want to use 'sendReport' to write the result data.
reportingBack
  :: (FilePath -> IO a)
  -- ^ Decode information stored in given file. If this fails,
  -- 'DecodeFailed' is thrown. Original exception is currently lost.
  -> (FilePath -> IO ())
  -- ^ Act with given temporary file path.
  -> IO a
reportingBack decode act = do
  prog <- getProgName
  withSystemTempDirectory prog $ \tmpDir -> do
    withTempFile tmpDir "address" $ \addrFile addrFileH -> do
      let tmpFileEvent (Added fp _ _) = fp == addrFile
          tmpFileEvent (Modified fp _ _) = fp == addrFile
          tmpFileEvent (Unknown fp _ _) = fp == addrFile
          tmpFileEvent _ = False
      withManager $ \mgr -> do
        result <- newEmptyMVar
        _ <- watchDir mgr tmpDir tmpFileEvent $ \_ -> do
          bracketOnError (pure ())
            (\() -> putMVar result Nothing)
            (\() -> decode addrFile >>= putMVar result . Just)
        act addrFile
        takeMVar result >>= \case
          Nothing -> throwIO DecodeFailed
          Just x -> pure x

-- | Should be coupled with 'reportingBack'. This function does a
-- write to temporary file in same directory followed by a copy. This
-- can be better than just a straight-forward write as copying via
-- 'copyFile' is atomic while writes are not.
sendReport :: FilePath -> ByteString -> IO ()
sendReport addrFile content = do
  withTempFile (takeDirectory addrFile) "report" $ \tmpFile h -> do
    BS.hPut h content
    hClose h
    copyFile tmpFile addrFile
