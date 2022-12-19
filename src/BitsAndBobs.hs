{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module BitsAndBobs (module BitsAndBobs) where

import Control.Monad.ST (ST, runST)
import Data.Bits
       ( complement
       , setBit
       , testBit
       , unsafeShiftL
       , unsafeShiftR
       , xor
       , (.&.)
       )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Data.Coerce (coerce)
import Data.Int (Int8)
import Data.Kind (Type)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Vector.Storable.Mutable (STVector)
import qualified Data.Vector.Storable.Mutable as MV
import Data.Word (Word32, Word64, Word8, byteSwap32)
import GHC.ByteOrder
       (ByteOrder(BigEndian, LittleEndian), targetByteOrder)
import Test.QuickCheck (Arbitrary, elements, arbitrary, choose)

------------------------------------------------------------------------

newtype Size = Size Int
  deriving newtype Num

data Endianness = Big | Little | Native

data Ty (t :: Type) :: Type where
  Bool       :: Ty Bool
  Word8      :: Size -> Ty Word8
--  Word16     :: Size -> Endianness -> Ty Word16
  Word32     :: Size -> Endianness -> Ty Word32
--  Word64     :: Size -> Endianness -> Ty Word64
  Int8       :: Size -> Ty Int8
--  Int16      :: Size -> Endianness -> Ty Int16
--  Int32      :: Size -> Endianness -> Ty Int32
--  Int64      :: Size -> Endianness -> Ty Int64
  Float      :: Size -> Endianness -> Ty Float
--  Double     :: Size -> Endianness -> Ty Double
--  BitString  :: Size -> Ty BitString
  ByteString :: Maybe Size -> Ty ByteString
-- Utf8 :: Size -> Ty Text

newtype BitString = MkBitString (Vector Bool)

data Segment = forall t. !t :. !(Ty t)

byteString :: [Segment] -> ByteString
byteString ss
  | segmentsBitSize ss `mod` 8 == 0 = bitsToByteString (segmentsBitSize ss) (segmentsToBits ss)
  | otherwise                       = error "byteString: the sum of all segment sizes must be a multiple of 8"

segmentsToBits :: [Segment] -> Vector Bool
segmentsToBits ss0 = runST $ do
  mv <- MV.unsafeNew (segmentsBitSize ss0)
  go 0 ss0 mv
  V.unsafeFreeze mv
  where
    go :: Int -> [Segment] -> STVector s Bool -> ST s ()
    go _offset []       _mv = return ()
    go  offset (s : ss)  mv = do
      segmentToBits' offset mv s
      go (offset + segmentBitSize s) ss mv

segmentToBits' :: Int -> STVector s Bool -> Segment -> ST s ()
segmentToBits' offset mbits s =
  V.imapM_ (\ix b -> MV.unsafeWrite mbits (offset + ix) b) (segmentToBits s)

segmentToBits :: Segment -> Vector Bool
segmentToBits (b  :. Bool)                 = V.singleton b
segmentToBits (w  :. Word8 sz)             = word8ToBits (coerce sz) w
segmentToBits (w  :. Word32 sz end)        = word32ToBits (coerce sz) end w
segmentToBits (i  :. Int8 sz)              = int8ToBits  (coerce sz) i
segmentToBits (f  :. Float sz end)         = word32ToBits (coerce sz) end (floatToWord32 f)
segmentToBits (bs :. ByteString Nothing)   = segmentsToBits $ map (\w -> w :. word8) $ BS.unpack bs
segmentToBits (bs :. ByteString (Just sz)) = segmentsToBits $ map (\w -> w :. word8) $ BS.unpack (BS.take (coerce sz) bs)
  -- XXX: cast from BS to Vec Bool?
  -- https://hackage.haskell.org/package/vector-0.13.0.0/docs/Data-Vector-Storable-Mutable.html#v:unsafeCast

word8ToBits :: Int -> Word8 -> Vector Bool
word8ToBits n w = V.generate n (\ix -> testBit w (n - 1 - ix))

word32ToBits :: Int -> Endianness -> Word32 -> Vector Bool
word32ToBits n Little w = word32ToBits n Big (byteSwap32 w)
word32ToBits n Big    w = V.generate n (\ix -> testBit w (n - 1 - ix))
word32ToBits n Native w = case targetByteOrder of
  BigEndian    -> word32ToBits n Big w
  LittleEndian -> word32ToBits n Little w

int8ToBits :: Int -> Int8 -> Vector Bool
int8ToBits n i = word8ToBits n (zigzag i)

zigzag :: Int8 -> Word8
zigzag i = fromIntegral ((i `unsafeShiftL` 1) `xor` (i `unsafeShiftR` (sizeOfInt8 - 1)))
  where
    sizeOfInt8 :: Int
    sizeOfInt8 = 8

zagzig :: Word8 -> Int8
zagzig w = fromIntegral ((w `unsafeShiftR` 1) `xor` (complement (w .&. 1) + 1))

prop_zigzag :: Int8 -> Bool
prop_zigzag i = zagzig (zigzag i) == i

prop_zagzig :: Word8 -> Bool
prop_zagzig w = zigzag (zagzig w) == w

floatToWord32 :: Float -> Word32
floatToWord32 f = V.unsafeCast (V.singleton f) V.! 0

doubleToWord64 :: Double -> Word64
doubleToWord64 d = V.unsafeCast (V.singleton d) V.! 0

word32ToFloat :: Word32 -> Float
word32ToFloat w = V.unsafeCast (V.singleton w) V.! 0

word64ToDouble :: Word64 -> Double
word64ToDouble w = V.unsafeCast (V.singleton w) V.! 0

prop_word32ToFloat :: Word32 -> Bool
prop_word32ToFloat w = floatToWord32 (word32ToFloat w) == w

prop_floatToWord32 :: Float -> Bool
prop_floatToWord32 f = word32ToFloat (floatToWord32 f) == f

prop_word64ToDouble :: Word64 -> Bool
prop_word64ToDouble w = doubleToWord64 (word64ToDouble w) == w

prop_doubleToWord64 :: Double -> Bool
prop_doubleToWord64 f = word64ToDouble (doubleToWord64 f) == f

segmentsBitSize :: [Segment] -> Int
segmentsBitSize = sum . map segmentBitSize

segmentBitSize :: Segment -> Int
segmentBitSize (_b :. Bool)                 = 1
segmentBitSize (_w :. Word8 sz)             = coerce sz
segmentBitSize (_w :. Word32 sz _end)       = coerce sz
segmentBitSize (_i :. Int8  sz)             = coerce sz
segmentBitSize (_f :. Float sz _end)        = coerce sz
segmentBitSize (bs :. ByteString Nothing)   = BS.length bs * 8
segmentBitSize (bs :. ByteString (Just sz)) = min (coerce sz * 8) (BS.length bs * 8)

bitsToByteString :: Int -> Vector Bool -> ByteString
bitsToByteString n
  = uncurry BS.fromForeignPtr0
  . V.unsafeToForeignPtr0
  . bitsToVecWord8
  . V.take n
  where
    bitsToVecWord8 :: Vector Bool -> Vector Word8
    bitsToVecWord8 bits = runST $ do
      ws <- MV.unsafeNew len
      go 0 ws
      V.unsafeFreeze ws
      where
        len :: Int
        len = V.length bits `div` 8

        go :: Int -> STVector s Word8 -> ST s ()
        go ix ws
          | ix == len = return ()
          | otherwise = do
              MV.unsafeWrite ws ix (bitsToWord8 (V.take 8 (V.drop (8 * ix) bits)))
              go (ix + 1) ws

bitsToWord8 :: Vector Bool -> Word8
bitsToWord8 bits = V.ifoldl' go 0 bits
  where
    len :: Int
    len = V.length bits - 1

    go :: Word8 -> Int -> Bool -> Word8
    go ih  ix True  = setBit ih (len - ix)
    go ih _ix False = ih

bitsToWord32 :: Endianness -> Vector Bool -> Word32
bitsToWord32 Big bits = V.ifoldl' go 0 bits
  where
    len :: Int
    len = V.length bits - 1

    go :: Word32 -> Int -> Bool -> Word32
    go ih  ix True  = setBit ih (len - ix)
    go ih _ix False = ih
bitsToWord32 Little bits =
  -- XXX: More efficient version...
  bitsToWord32 Big (V.fromList (concat (reverse (splitEvery 8 (V.toList bits)))))
  where
    splitEvery :: Int -> [a] -> [[a]]
    splitEvery _ [] = []
    splitEvery n xs = as : splitEvery n bs
      where (as, bs) = splitAt n xs
bitsToWord32 Native bits = case targetByteOrder of
  BigEndian    -> bitsToWord32 Big bits
  LittleEndian -> bitsToWord32 Little bits

exampleByteString :: ByteString
exampleByteString =
  let
    red   = 2
    green = 61
    blue  = 20
  in
    byteString [ red :. sized word8 5, green :. sized word8 6, blue :. sized word8 5]

exampleByteString' :: ByteString
exampleByteString' = byteString [5 :. word8, "hello, rest" :. bytes]

exampleByteString'' :: ByteString
exampleByteString'' = byteString [(-2) :. sized int8 5, (-31) :. sized int8 6, (-15) :. sized int8 5]

------------------------------------------------------------------------

infixr 5 :::
infixl 1 :>>=
data Pattern (ts :: [Type]) :: Type where
  Nil    :: Pattern '[]
  (:::)  :: Ty t -> Pattern ts -> Pattern (t ': ts)
  (:>>=) :: Integral i => Ty i -> (i -> Pattern ts) -> Pattern (i ': ts)

type family   Match (ts :: [Type]) :: Type
type instance Match '[]       = ()
type instance Match (t ': ts) = (t, Match ts)

bitMatch :: Pattern ts -> ByteString -> Match ts
bitMatch ps0 = go ps0 . segmentToBits . (:. bytes)
  where
    go :: Pattern ts -> Vector Bool -> Match ts
    go Nil       _bits = ()
    go (p ::: ps) bits =
      let
        (t, n) = match1 p bits
      in
        (t, go ps (V.drop n bits))
    go (p :>>= k) bits =
      let
        (i, n)  = match1 p bits
      in
        (i, go (k i) (V.drop n bits))

match1 :: Ty t -> Vector Bool -> (t, Int)
match1 Bool                   bits = (V.head bits, 1) -- XXX: partial...
match1 (Word8 sz)             bits = (bitsToWord8 (V.take (coerce sz) bits), coerce sz)
match1 (Word32 sz end)        bits = (bitsToWord32 end (V.take (coerce sz) bits), coerce sz)
match1 (Int8 sz)              bits = (bitsToInt8 (V.take (coerce sz) bits), coerce sz)
match1 (Float sz end)         bits = (word32ToFloat (bitsToWord32 end (V.take (coerce sz) bits)), coerce sz)
match1 (ByteString Nothing)   bits = (bitsToByteString (V.length bits) bits, V.length bits)
match1 (ByteString (Just sz)) bits = (bitsToByteString (coerce sz) bits, coerce sz)

bitsToInt8 :: Vector Bool -> Int8
bitsToInt8 bits = zagzig (bitsToWord8 bits)

data Int8ToBits = Int8ToBits Int Int8
  deriving stock Show

instance Arbitrary Int8ToBits where
  arbitrary = do
    sz <- elements [1..8]
    i  <- choose (-(2^(sz - 1) - 1), 2^(sz-1) - 1)
    return (Int8ToBits sz i)

prop_int8ToBits :: Int8ToBits -> Bool
prop_int8ToBits (Int8ToBits sz i) = bitsToInt8 (V.take sz (int8ToBits sz i)) == i

------------------------------------------------------------------------

examplePattern :: Pattern '[Word8, Word8, Word8]
examplePattern = sized word8 5 ::: sized word8 6 ::: sized word8 5 ::: Nil

test :: (Word8, (Word8, (Word8, ())))
test = bitMatch examplePattern exampleByteString

examplePattern' :: Pattern '[Word8, ByteString, ByteString]
examplePattern' = word8 :>>= \sz -> sized bytes sz ::: bytes ::: Nil

test' :: (Word8, (ByteString, (ByteString, ())))
test' = bitMatch examplePattern' exampleByteString'

examplePattern'' :: Pattern '[Int8, Int8, Int8]
examplePattern'' = sized int8 5 ::: sized int8 6 ::: sized int8 5 ::: Nil

test'' :: (Int8, (Int8, (Int8, ())))
test'' = bitMatch examplePattern'' exampleByteString''

exampleByteStringBools :: ByteString
exampleByteStringBools = byteString
  [ True :. Bool, False :. Bool, True :. Bool, False :. Bool
  , True :. Bool, True :. Bool, True :. Bool, True :. Bool
  ]

testBools :: (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, (Bool, ()))))))))
testBools = bitMatch (Bool ::: Bool ::: Bool ::: Bool :::
                      Bool ::: Bool ::: Bool ::: Bool ::: Nil)
              exampleByteStringBools

testBools' :: (Word8, ())
testBools' = bitMatch (word8 ::: Nil) exampleByteStringBools

------------------------------------------------------------------------

word8 :: Ty Word8
word8 = Word8 8

word32 :: Ty Word32
word32 = Word32 32 Big

int8 :: Ty Int8
int8 = Int8 8

float :: Ty Float
float = Float 32 Big

bytes :: Ty ByteString
bytes = ByteString Nothing

sized :: Integral i => Ty a -> i -> Ty a
sized Bool             _sz = Bool
sized (Word8 _sz)       sz = Word8 (fromIntegral sz)
sized (Word32 _sz end)  sz = Word32 (fromIntegral sz) end
sized (Int8 _sz)        sz = Int8 (fromIntegral sz)
sized (Float _sz end)   sz = Float (fromIntegral sz) end
sized (ByteString _mSz) sz = ByteString (Just (fromIntegral sz * 8))

little :: Ty a -> Ty a
little (Word32 sz _end) = Word32 sz Little
little (Float  sz _end) = Float  sz Little
little x                = x

------------------------------------------------------------------------

testEndianness, testEndiannessLE :: [Word8]
testEndianness   = BS.unpack $ byteString [0x12345678 :. word32 ]
testEndiannessLE = BS.unpack $ byteString [0x12345678 :. little word32 ]

testMatchEndianness, testMatchEndiannessLE :: (Word32, ())
testMatchEndianness   = bitMatch (Word32 32 Big    ::: Nil) (byteString [0x12345678 :. word32 ])
testMatchEndiannessLE = bitMatch (Word32 32 Little ::: Nil) (byteString [0x12345678 :. little word32 ])

codecFloat :: (Float, ())
codecFloat = bitMatch (float ::: Nil) (byteString [0.12345678 :. float])

codecFloatLE :: (Float, ())
codecFloatLE = bitMatch (little float ::: Nil) (byteString [0.12345678 :. little float])

------------------------------------------------------------------------

w8 :: Word8 -> String
w8 = ("0b" ++) . concatMap show . go []
  where
   go :: [Word8] -> Word8 -> [Word8]
   go acc 0 = replicate (8 - length acc) 0 ++ acc
   go acc n = let (d, m) = divMod2 n in go (m : acc) d

i8 :: Int8 -> String
i8 = w8 . fromIntegral

div2 :: Word8 -> Word8
div2 w = w `unsafeShiftR` 1

mod2 :: Word8 -> Word8
mod2 w = w .&. 1

divMod2 :: Word8 -> (Word8, Word8)
divMod2 w = (div2 w, mod2 w)
