{-# LANGUAGE StrictData #-}

module BitsAndBobs.Block (module BitsAndBobs.Block) where

import Control.Exception
import Foreign
import Foreign.C.Types
import System.Posix.IO
import System.Posix.Files
import System.IO

import BitsAndBobs.Mmap

------------------------------------------------------------------------

data Block a = Block
  { blockPtr    :: Ptr a
  , blockLength :: CSize
  }

mmapFile :: FilePath -> IO (Block a)
mmapFile fp = do
  let mFileMode = Just (ownerReadMode `unionFileModes` ownerWriteMode)
  bracket (openFd fp ReadWrite mFileMode defaultFileFlags) closeFd $ \fd -> do
    len <- fromIntegral <$> fdSeek fd SeekFromEnd 0
    ptr <- mmap Nothing len (pROT_READ .|. pROT_WRITE) mAP_SHARED (Just fd) 0
    return (Block ptr (fromIntegral len))

allocaBlock :: Int -> (Block a -> IO b) -> IO b
allocaBlock len k = allocaBytes len $ \ptr -> k (Block ptr (fromIntegral len))
