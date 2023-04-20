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
import BitsAndBobs.Codec
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
  Right expected @=? decode exampleSchemaGeneric block

unit_genericEncode :: Assertion
unit_genericEncode = allocaBlock 9 $ \block -> do
  let expected = Example 4 "hello"
  Right () <- encode exampleSchemaGeneric expected block
  Right expected @=? decode exampleSchemaGeneric block

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
  Right i @=? j

unit_decodeBS :: Assertion
unit_decodeBS = allocaBytes (sizeOf (4 :: Int32) + length ("hello" :: String)) $ \ptr -> do
  let len :: Int32
      len = 5
  poke ptr len
  let hello = map BS.c2w "hello"
  pokeArray (ptr `plusPtr` sizeOf (4 :: Int32)) hello
  let bs = decodeAccessor exampleSchema (Field "string") (Block ptr 9)
  Right (BS.pack hello) @=? bs
