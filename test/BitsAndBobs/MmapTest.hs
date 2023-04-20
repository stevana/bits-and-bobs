module BitsAndBobs.MmapTest where

import Test.Tasty.HUnit
import Data.Bits
import Foreign

import BitsAndBobs.Mmap

------------------------------------------------------------------------

unit_mmap :: Assertion
unit_mmap = do
  pageSize <- sysconfPageSize
  fptr <- posixMemalign pageSize pageSize
  withForeignPtr fptr $ \ptr' -> do
    ptr <- mmap (Just ptr') 16 (pROT_READ .|. pROT_WRITE) mAP_SHARED Nothing 0
    if ptr /= ptr'
    then error "not same ptr"
    else do
      msync ptr 16 mS_SYNC False
      munmap ptr 16
