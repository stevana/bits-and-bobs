{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StrictData #-}

module BitsAndBobs.Block (module BitsAndBobs.Block) where

import Control.Exception
import Foreign
import Foreign.C.Types
import Foreign.Marshal.Alloc (finalizerFree)
import GHC.Exts
import System.IO
import System.Posix.Files
import System.Posix.IO

import BitsAndBobs.Mmap

------------------------------------------------------------------------

data Block a = Block
  { blockPtr    :: Ptr a -- ForeignPtr a
  , blockLength :: CSize
  }

mmapFile :: FilePath -> IO (Block a)
mmapFile fp = do
  let mFileMode = Just (ownerReadMode `unionFileModes` ownerWriteMode)
  bracket (openFd fp ReadWrite mFileMode defaultFileFlags) closeFd $ \fd -> do
    len <- fromIntegral <$> fdSeek fd SeekFromEnd 0
    ptr <- mmap Nothing len (pROT_READ .|. pROT_WRITE) mAP_SHARED (Just fd) 0
    -- fptr <- newForeignPtr finalizerFree ptr
    return (Block ptr (fromIntegral len))

allocaBlock :: Int -> (Block a -> IO b) -> IO b
allocaBlock len k = allocaBytes len $ \ptr -> k (Block ptr (fromIntegral len))
  -- fptr <- mallocForeignPtrBytes len
  -- k (Block fptr (fromIntegral len))

withBlockAddr :: Block a -> (Addr# -> b) -> b
withBlockAddr block k = k addr#
  where
    !(Ptr addr#) = blockPtr block

stringBlock :: String -> Block Char
stringBlock s = undefined
