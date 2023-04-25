{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StrictData #-}

module BitsAndBobs.Block (module BitsAndBobs.Block) where

import GHC.IO (IO(IO))
import GHC.Int
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.ByteString (ByteString)
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import qualified Data.ByteString.Internal as BS
import Control.Exception
import Foreign
import Foreign.C.Types
import Foreign.Marshal.Alloc (finalizerFree)
import GHC.Exts
import System.IO
import System.Posix.Files
import System.Posix.IO
import Text.Printf

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
    -- See mmapped-metrics/src/Block
    -- fptr <- newForeignPtr finalizerFree ptr
    return (Block ptr (fromIntegral len))

allocaBlock :: Int -> (Block a -> IO b) -> IO b
allocaBlock len k = allocaBytes len $ \ptr -> k (Block ptr (fromIntegral len))
  -- fptr <- mallocForeignPtrBytes len
  -- k (Block fptr (fromIntegral len))

callocBlock :: Int -> (Block a -> IO b) -> IO b
callocBlock len k = bracket (callocBytes len) free $ \ptr ->
  k (Block ptr (fromIntegral len))

withBlockAddr :: Block a -> (Addr# -> b) -> b
withBlockAddr block k = k addr#
  where
    !(Ptr addr#) = blockPtr block

stringBlock :: String -> Block Char
stringBlock s = undefined

blockToByteString :: Block a -> ByteString
blockToByteString block =
  BS.unsafeCreate len (\ptr -> copyBytes ptr (castPtr (blockPtr block)) len)
  where
    len = fromIntegral (blockLength block)

writeInt32 :: Block a -> Int -> Int32 -> IO ()
writeInt32 block (I# offset#) (I32# i32#) =
  withBlockAddr block $ \addr# ->
    IO $ \s ->
      (# writeInt32OffAddr# (addr# `plusAddr#` offset#) 0# i32# s, () #)

readInt32 :: Block a -> Int -> Int32
readInt32 block (I# offset#) =
  withBlockAddr block $ \addr# ->
    I32# (indexInt32OffAddr# (addr# `plusAddr#` offset#) 0#)

------------------------------------------------------------------------

-- * Debugging

displayBlock :: Block a -> LBS.ByteString
displayBlock block = do
  let bs  = blockToByteString block
      lbs = toLazyByteString (byteStringHex bs)
  LBS.intercalate (fromString "\n") (reverse (go 0 [] (splitEvery 4 lbs)))
  where
    go :: Int -> [LBS.ByteString] -> [LBS.ByteString] -> [LBS.ByteString]
    go offset acc [] = LBS8.pack (printf "%07d\n" offset) : acc
    go offset acc xs = do
      let lbs = fromString (printf "%07d " offset) <> LBS.intercalate (fromString " ") (take 8 xs)
      go (offset + 16) (lbs : acc) (drop 8 xs)

    splitEvery :: Int64 -> LBS.ByteString -> [LBS.ByteString]
    splitEvery n lbs | LBS.null lbs = []
                     | otherwise    = l : splitEvery n r
      where
        (l, r) = LBS.splitAt n lbs

printBlock :: Block a -> IO ()
printBlock = LBS8.putStrLn . displayBlock
