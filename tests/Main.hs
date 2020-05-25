{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import System.Exit
import System.Process.ReportBack

-- | Acts as a simple "process" that just takes filepath to write to
-- and writes some info.
writeProcess :: FilePath -> ByteString -> IO ()
writeProcess fp content = sendReport fp content

main :: IO ()
main = do
  results <- forM contents $ \c -> reportingBack BS.readFile $ \fp ->
    writeProcess fp c
  when (contents /= results) $ do
    putStrLn $ unwords [ "Expected", show contents, "but got", show results ]
    exitFailure
  where
    contents :: [ByteString]
    contents = ["foo", "bar", "baz"]
