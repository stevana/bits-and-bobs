{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module BitsAndBobs.Generic where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import Data.Int
import Data.String
import GHC.Generics
import Foreign

import BitsAndBobs.Block
import BitsAndBobs.Schema

------------------------------------------------------------------------

class Decode a where
  decode :: Schema -> Block b -> Either DecodeError a
  default decode :: (Generic a, Decode1 (Rep a)) => Schema -> Block b -> Either DecodeError a
  decode schema block = fmap to (decode1 schema block)

class Decode1 f where
  decode1 :: Schema -> Block b -> Either DecodeError (f p)

instance Decode1 f => Decode1 (M1 D c f) where
  decode1 schema block = fmap M1 (decode1 schema block)

instance Decode1 f => Decode1 (M1 C c f) where
  decode1 schema block = fmap M1 (decode1 schema block)

instance (Selector s, Codec a) => Decode1 (M1 S s (K1 i a)) where
  decode1 schema block =
    let
      m :: M1 i s f a
      m = undefined
    in
      fmap (M1 . K1) (decodeField schema (fromString (selName m)) block)

instance (Decode1 f, Decode1 g) => Decode1 (f :*: g) where
  decode1 schema block = liftA2 (:*:) (decode1 schema block) (decode1 schema block)

------------------------------------------------------------------------

data Example = Example { foo :: Int32, bar :: ByteString }
  deriving (Show, Generic)

instance Decode Example

exampleSchemaGeneric :: Schema
exampleSchemaGeneric = newSchema
  [ ("foo",  Int32)
  , ("bar",  ByteString (Fixed 5))
  ]

unit_genericDecode :: IO (Either DecodeError Example)
unit_genericDecode = allocaBlock 9 $ \block -> do
  let ptr = blockPtr block
  poke ptr (4 :: Int32)
  let hello = map BS.c2w "hello"
  pokeArray (ptr `plusPtr` sizeOf (4 :: Int32)) hello
  return (decode exampleSchemaGeneric block)
