module BitsAndBobs.Mmap (module BitsAndBobs.Mmap) where

import Data.Bits ((.|.))
import Foreign.Concurrent (newForeignPtr)
import Foreign.C.Error
import Data.Maybe (fromMaybe)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc
import Foreign.Storable
import System.Posix.Types

------------------------------------------------------------------------

foreign import ccall unsafe "sys/mman.h mmap"
  c_mmap :: Ptr a -> CSize -> CInt -> CInt -> CInt -> COff -> IO (Ptr a)

foreign import ccall unsafe "sys/mman.h munmap"
  c_munmap :: Ptr a -> CSize -> IO CInt

foreign import ccall unsafe "sys/mman.h msync"
  c_msync :: Ptr a -> CSize -> CInt -> IO CInt

foreign import ccall unsafe "stdlib.h posix_memalign"
  c_posix_memalign :: Ptr (Ptr a) -> CSize -> CSize -> IO CInt

foreign import ccall unsafe "unistd.h sysconf"
  c_sysconf :: CInt -> IO CLong


#include <unistd.h>
_SC_PAGE_SIZE :: CInt
_SC_PAGE_SIZE = #const _SC_PAGE_SIZE

#include <sys/mman.h>
mAP_ANONYMOUS, mAP_FIXED :: CInt
mAP_ANONYMOUS = #const MAP_ANONYMOUS
mAP_FIXED     = #const MAP_FIXED

mAP_SHARED, mAP_PRIVATE :: CInt
mAP_SHARED    = #const MAP_SHARED
mAP_PRIVATE   = #const MAP_PRIVATE

pROT_READ, pROT_WRITE, pROT_EXEC, pROT_NONE :: CInt
pROT_READ     = #const PROT_READ
pROT_WRITE    = #const PROT_WRITE
pROT_EXEC     = #const PROT_EXEC
pROT_NONE     = #const PROT_NONE

mS_ASYNC, mS_SYNC, mS_INVALIDATE :: CInt
mS_ASYNC      = #const MS_ASYNC
mS_SYNC       = #const MS_SYNC
mS_INVALIDATE = #const MS_INVALIDATE

------------------------------------------------------------------------

mmap :: Maybe (Ptr a) -> Int -> CInt -> CInt -> Maybe Fd
     -> COff -> IO (Ptr a)
mmap mAddr len prot visib mFd offset =
  throwErrnoIf (== mAP_FAILED) "mmap" $
    c_mmap addr (fromIntegral len) prot flags fd offset
  where
    mAP_FAILED = nullPtr `plusPtr` (-1)

    addr = fromMaybe nullPtr mAddr

    fd :: CInt
    fd = case mFd of
      Nothing        -> (-1)
      Just (Fd cint) -> cint

    flags :: CInt
    flags =  maybe mAP_ANONYMOUS (const 0) mFd
         .|. maybe 0 (const mAP_FIXED) mAddr
         .|. visib

munmap :: Ptr a -> CSize -> IO ()
munmap addr len = throwErrnoIfMinus1_ "munmap" (c_munmap addr len)

msync
  :: Ptr a
  -> CSize
  -> CInt
  -> Bool -- ^ Asks to invalidate other mappings of the same file (so
          --   that they can be updated with the fresh values just
          --   written).
  -> IO ()
msync addr len syncFlag invalidate = throwErrnoIfMinus1_ "msync" (c_msync addr len flags)
  where
    flags = syncFlag .|. if invalidate then mS_INVALIDATE else 0

posixMemalignFPtr :: Int -> Int -> IO (ForeignPtr a)
posixMemalignFPtr align size = do
  memPtr <- malloc
  throwErrnoIfMinus1_ "posix_memalign"
    (c_posix_memalign memPtr (fromIntegral align) (fromIntegral size))
  ptr <- peek memPtr
  newForeignPtr ptr (finalizer memPtr ptr)
  where
    finalizer :: Ptr (Ptr a) -> Ptr a -> IO ()
    finalizer memPtr ptr = do
      free memPtr
      free ptr

posixMemalign :: Int -> Int -> IO (Ptr a)
posixMemalign align size = do
  memPtr <- malloc
  throwErrnoIfMinus1_ "posix_memalign"
    (c_posix_memalign memPtr (fromIntegral align) (fromIntegral size))
  peek memPtr

sysconfPageSize :: IO Int
sysconfPageSize = fromIntegral <$> c_sysconf _SC_PAGE_SIZE

------------------------------------------------------------------------

main :: IO ()
main = do
  fptr <- posixMemalignFPtr 4096 4096
  withForeignPtr fptr $ \ptr' -> do
    ptr <- mmap (Just ptr') 16 (pROT_READ .|. pROT_WRITE) mAP_SHARED Nothing 0
    if ptr /= ptr'
    then error "not same ptr"
    else do
      msync ptr 16 mS_SYNC False
      munmap ptr 16
