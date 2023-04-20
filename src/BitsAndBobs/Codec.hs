{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}

module BitsAndBobs.Codec (module BitsAndBobs.Codec) where

import Control.Applicative
import Data.ByteString
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Foreign
import Foreign.Marshal.Utils (copyBytes)
import GHC.Exts
import GHC.Generics
import GHC.Int
import GHC.IO (IO(IO))
import Text.Read

import BitsAndBobs.Accessor
import BitsAndBobs.Block
import BitsAndBobs.Schema

------------------------------------------------------------------------

class Encode a where
  encode :: Schema -> a -> Block b -> IO (Either EncodeError ())

  default encode :: (Generic a, Encode1 (Rep a))
                 => Schema -> a -> Block b -> IO (Either EncodeError ())
  encode = genericEncode

class Decode a where
  decode :: Schema -> Block b -> Either DecodeError a

  default decode :: (Generic a, Decode1 (Rep a))
                 => Schema -> Block b -> Either DecodeError a
  decode = genericDecode

verifyMagic :: Schema -> Block a -> IO Bool
verifyMagic schema block = go True (Map.toList (schemaTypes schema))
  where
    go False _                          = return False
    go _acc []                          = return True
    go acc ((field, Magic magic) : fts) = do
      let eBs = decodeAccessor schema (Field field) block
      case eBs of
        Left _decodeError -> return False
        Right bs          -> go (bs == magic && acc)fts
    go acc ((_field, _type) : fts) = go acc fts

data DecodeError
  = WrongField Field
  | WrongType Type
  | WrongLengthType Type
  | WrongLengthField Field
  | FieldUnknownOffset Field
  | LengthFieldUnknownOffset Field
--  | TypeError AccessorTypeError
  deriving stock (Eq, Show)

data Value = ByteStringV ByteString
  deriving Show

prettyValue :: Value -> Text
prettyValue (ByteStringV bs) = Text.decodeUtf8Lenient bs

readValue :: Schema -> Field -> Text -> Either String Value
readValue schema field text =
  case lookupFieldType field schema of
    Nothing -> Left "field isn't in schema"
    Just ty ->
      case ty of
        ByteString _ -> ByteStringV <$> readEither (Text.unpack text) -- XXX: use parser?

decodeField' :: Schema -> Field -> Block a -> Either DecodeError Value
decodeField' schema field block =
  case lookupFieldType field schema of
    Nothing -> Left (WrongField field)
    Just (ByteString _) -> ByteStringV <$> decodeAccessor schema (Field field) block

encodeField' :: Schema -> Field -> Block a -> Value -> IO (Either EncodeError ())
encodeField' schema field block (ByteStringV bs) = encodeAccessor schema (Field field) block bs


encodeAccessor :: EncodeValue a => Schema -> Accessor -> Block b -> a -> IO (Either EncodeError ())
encodeAccessor schema accessor block x =
  case typeCheckAccessor schema accessor of
    Left err -> Left <$> error "EncodeTypeError err"
    Right () -> case boundCheck schema accessor block of
      Left err -> Left <$> error "EncodeBoundCheck err"
      Right () -> Right <$> encodeValue x (access schema accessor block)

decodeAccessor :: DecodeValue a => Schema -> Accessor -> Block b -> Either DecodeError a
decodeAccessor schema accessor block =
  case typeCheckAccessor schema accessor of
    Left err -> Left (error "DecodeTypeError err")
    Right () -> case boundCheck schema accessor block of
      Left err -> Left (error "BoundCheck err")
      Right () -> Right (decodeValue (access schema accessor block))

sizeOfType' :: Schema -> Accessor -> Block a -> Int32
sizeOfType' schema accessor block = decodeValue (access schema accessor block)

access :: Schema -> Accessor -> Block a -> Block b
access schema accessor block = Block ptr (fromIntegral len)
  where
    offset = accessorOffset (Record schema) accessor
    ptr | offset >= 0 = castPtr (blockPtr block `plusPtr` offset)
        | otherwise   = castPtr (blockPtr block `plusPtr` fromIntegral (blockLength block) `plusPtr` offset)
    len = case lookupAccessorType (Record schema) accessor of
      ByteString (Variable lenField) -> fromIntegral (sizeOfType' schema (Field lenField) block)
      ty -> sizeOfType_ ty

class EncodeValue a where
  encodeValue :: a -> Block b -> IO ()

instance EncodeValue Int32 where
  encodeValue (I32# i32#) block =
    withBlockAddr block $ \addr# ->
      IO $ \s ->
        (# writeInt32OffAddr# addr# 0# i32# s, () #)

instance EncodeValue ByteString where
  encodeValue bs block =
    -- This is safe because we are not changing `src`.
    BS.unsafeUseAsCStringLen bs $ \(src, len) ->
      copyBytes (blockPtr block) (castPtr src) len

class DecodeValue a where
  decodeValue :: Block b -> a

instance DecodeValue Int32 where
  decodeValue block = withBlockAddr block $ \addr# ->
    I32# (indexInt32OffAddr# addr# 0#)

instance DecodeValue ByteString where
  decodeValue block =
    BS.unsafeCreate len (\ptr -> copyBytes ptr (castPtr (blockPtr block)) len)
    where
      len = fromIntegral (blockLength block)

------------------------------------------------------------------------

genericDecode :: (Generic a, Decode1 (Rep a)) => Schema -> Block b -> Either DecodeError a
genericDecode schema block = fmap to (decode1 schema block)

class Decode1 f where
  decode1 :: Schema -> Block b -> Either DecodeError (f p)

instance Decode1 f => Decode1 (M1 D c f) where
  decode1 schema block = fmap M1 (decode1 schema block)

instance Decode1 f => Decode1 (M1 C c f) where
  decode1 schema block = fmap M1 (decode1 schema block)

instance (Selector s, DecodeValue a) => Decode1 (M1 S s (K1 i a)) where
  decode1 schema block =
    let
      m :: M1 i s f a
      m = undefined
    in
      fmap (M1 . K1) (decodeAccessor schema (Field (fromString (selName m))) block)

instance (Decode1 f, Decode1 g) => Decode1 (f :*: g) where
  decode1 schema block = liftA2 (:*:) (decode1 schema block) (decode1 schema block)

------------------------------------------------------------------------

genericEncode :: (Generic a, Encode1 (Rep a)) => Schema -> a -> Block b -> IO (Either EncodeError ())
genericEncode schema x block = encode1 schema (from x) block

class Encode1 f where
  encode1 :: Schema -> f p -> Block b -> IO (Either EncodeError ())

instance Encode1 f => Encode1 (M1 D c f) where
  encode1 schema (M1 x) block = encode1 schema x block

instance Encode1 f => Encode1 (M1 C c f) where
  encode1 schema (M1 x) block = encode1 schema x block

instance (Selector s, EncodeValue a) => Encode1 (M1 S s (K1 i a)) where
  encode1 schema (M1 (K1 x)) block =
    let
      m :: M1 i s f a
      m = undefined
    in
      encodeAccessor schema (Field (fromString (selName m))) block x

instance (Encode1 f, Encode1 g) => Encode1 (f :*: g) where
  encode1 schema (x :*: y) block = do
    encode1 schema x block
    encode1 schema y block
