{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module BitsAndBobs.CodecTest where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Data.Int
import Foreign
import GHC.Generics (Generic)
import Test.Tasty.HUnit

import BitsAndBobs.Accessor
import BitsAndBobs.Block
import BitsAndBobs.Codec (Decode, Encode, encode, decode, encodeAccessor, decodeAccessor)
import BitsAndBobs.Schema

------------------------------------------------------------------------

data Example = Example { foo :: Int32, bar :: ByteString }
  deriving (Eq, Show, Generic)

instance Decode Example
instance Encode Example

exampleSchemaGeneric :: Schema
exampleSchemaGeneric = newSchema
  [ ("foo",  Int32)
  , ("bar",  ByteString (Fixed 5))
  ]

unit_genericDecode :: Assertion
unit_genericDecode = allocaBlock 9 $ \block -> do
  let ptr = blockPtr block
  poke ptr (4 :: Int32)
  let hello = map BS.c2w "hello"
  pokeArray (ptr `plusPtr` sizeOf (4 :: Int32)) hello
  let expected = Example 4 "hello"
  expected @=? decode exampleSchemaGeneric Id block

unit_genericEncode :: Assertion
unit_genericEncode = allocaBlock 9 $ \block -> do
  let expected = Example 4 "hello"
  () <- encode exampleSchemaGeneric Id expected block
  expected @=? decode exampleSchemaGeneric Id block

exampleNestedSchema :: Schema
exampleNestedSchema = newSchema
  [ ("length", Int32)
  , ("array", Array (Fixed 10) (Record (newSchema [ ("foo", Int32)
                                                  , ("bar", Int32)
                                                  ])))
  ]

exampleSchema :: Schema
exampleSchema = newSchema [("length", Int32), ("string", ByteString (Variable "length"))]

unit_decodeInt32 :: Assertion
unit_decodeInt32 = allocaBytes 4 $ \ptr -> do
  let i :: Int32
      i = 1234
  poke ptr i
  let j = decodeAccessor exampleSchema (Field "length") (Block ptr 4)
  i @=? j

unit_decodeBS :: Assertion
unit_decodeBS = allocaBytes (sizeOf (4 :: Int32) + length ("hello" :: String)) $ \ptr -> do
  let len :: Int32
      len = 5
  poke ptr len
  let hello = map BS.c2w "hello"
  pokeArray (ptr `plusPtr` sizeOf (4 :: Int32)) hello
  let bs = decodeAccessor exampleSchema (Field "string") (Block ptr 9)
  (BS.pack hello) @=? bs

unit_decodeNested :: Assertion
unit_decodeNested = allocaBlock (4 + 10 * (4 + 4)) $ \block -> do
  let answer :: Int32
      answer = 42
  poke (blockPtr block `plusPtr` (4 + 2 * (4 + 4) + 4)) answer
  let r = decodeAccessor exampleNestedSchema (Field "array" :. Index 2 :. Field "bar") block
  answer @=? r

data NestedExample = NestedExample
  { len :: Int32
  , arr :: [Int32]
  }
  deriving (Eq, Show, Generic)

instance Encode NestedExample
instance Decode NestedExample

exampleNestedSchema' :: Schema
exampleNestedSchema' = newSchema
  [ ("len", Int32)
  , ("arr", Array (Variable "len") Int32)
  ]

unit_genericEncodeNested :: Assertion
unit_genericEncodeNested = allocaBlock 12 $ \block -> do
  let example = NestedExample 2 [1, 3]
  encode exampleNestedSchema' Id example block
  let bs = BS.unsafeCreate 12 $ \ptr -> do
             poke (castPtr ptr) (2 :: Int32)
             poke (castPtr ((ptr `plusPtr` 4))) (1 :: Int32)
             poke (castPtr ((ptr `plusPtr` 8))) (3 :: Int32)
  bs @=? blockToByteString block

unit_genericDecodeNested :: Assertion
unit_genericDecodeNested = allocaBlock 12 $ \block -> do
  let expected = NestedExample 2 [1, 3]
  encode exampleNestedSchema' Id expected block
  let actual = decode exampleNestedSchema' Id block
  expected @=? actual

data PairInt32 = PairInt32 { l :: Int32, r :: Int32 }
  deriving (Eq, Show, Generic)

instance Encode PairInt32
instance Decode PairInt32

exampleNestedSchema1 :: Schema
exampleNestedSchema1 = newSchema
  [ ("int", Int32)
  , ("record", (Record $ newSchema [("l", Int32), ("r", Int32)]))
  ]

data NestedExample1 = NestedExample1
  { int    :: Int32
  , record :: PairInt32
  }
  deriving (Eq, Show, Generic)

instance Encode NestedExample1
instance Decode NestedExample1

unit_nestedRecordEncode :: Assertion
unit_nestedRecordEncode = allocaBlock (4 + 8) $ \block -> do
  let expected = 3
      example  = NestedExample1 1 (PairInt32 2 expected)
  encodeAccessor exampleNestedSchema1 (Field "record" :. Field "r") block expected
  let actual = decodeAccessor exampleNestedSchema1 (Field "record" :. Field "r") block
  expected @=? actual

unit_nestedRecord1 :: Assertion
unit_nestedRecord1 = allocaBlock (4 + 8) $ \block -> do
  let expected = 3
      example  = NestedExample1 1 (PairInt32 2 expected)
  encode exampleNestedSchema1 Id example block
  let actual = decodeAccessor exampleNestedSchema1 (Field "record" :. Field "r") block
  expected @=? actual

exampleNestedSchema2 :: Schema
exampleNestedSchema2 = newSchema
  [ ("len2", Int32)
  , ("arr2", Array (Variable "len2") (Record $ newSchema [("l", Int32), ("r", Int32)]))
  ]

data NestedExample2 = NestedExample2
  { len2 :: Int32
  , arr2 :: [PairInt32]
  }
  deriving (Eq, Show, Generic)

instance Encode NestedExample2
instance Decode NestedExample2

unit_genericDecodeNestedAccessor :: Assertion
unit_genericDecodeNestedAccessor = allocaBlock (4 + 8 * 2) $ \block -> do
  let expected = 4
      example  = NestedExample2 2 [PairInt32 1 3, PairInt32 expected 5]
  encode exampleNestedSchema2 Id example block
  let actual = decodeAccessor exampleNestedSchema2 (Field "arr2" :. Index 1 :. Field "l") block
  expected @=? actual

unit_genericDecodeNested2 :: Assertion
unit_genericDecodeNested2 = allocaBlock (4 + 8 * 2) $ \block -> do
  let expected = NestedExample2 2 [PairInt32 1 3, PairInt32 4 5]
  encode exampleNestedSchema2 Id expected block
  let actual = decode exampleNestedSchema2 Id block
  expected @=? actual
