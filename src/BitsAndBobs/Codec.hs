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
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Debug.Trace
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
  encode :: Schema -> Accessor -> a -> Block b -> IO ()

  default encode :: (Generic a, Encode1 (Rep a))
                 => Schema -> Accessor -> a -> Block b -> IO ()
  encode = genericEncode

class Decode a where
  decode :: Schema -> Accessor -> Block b -> a

  default decode :: (Generic a, Decode1 (Rep a))
                 => Schema -> Accessor -> Block b -> a
  decode = genericDecode

------------------------------------------------------------------------

genericDecode :: (Generic a, Decode1 (Rep a)) => Schema -> Accessor -> Block b -> a
genericDecode schema accessor block = to (decode1 schema accessor block)

class Decode1 f where
  decode1 :: Schema -> Accessor -> Block b -> f p

instance Decode1 f => Decode1 (M1 D c f) where
  decode1 schema accessor block = M1 (decode1 schema accessor block)

instance Decode1 f => Decode1 (M1 C c f) where
  decode1 schema accessor block = M1 (decode1 schema accessor block)

instance (Selector s, Decode a) => Decode1 (M1 S s (K1 i [a])) where
  decode1 schema accessor block =
    let
      m :: M1 i s f a
      m = undefined
    in
      M1 (K1 (decodeArray schema (accessor :. Field (fromString (selName m))) block))
    where
      decodeArray :: Decode a => Schema -> Accessor -> Block b -> [a]
      decodeArray schema accessor block = go len []
        where
          len = accessorLength schema accessor block

          go n acc | n == 0    = acc
                   | otherwise =
                       let
                         x = decode schema (accessor :. Index (n - 1)) block
                       in
                         go (n - 1) (x : acc)

      accessorLength :: Schema -> Accessor -> Block b -> Int
      accessorLength schema accessor block =
        case lookupAccessorType (Record schema) accessor of
          Array      (Fixed len) _ty -> len
          ByteString (Fixed len)     -> len

          Array      (Variable lenField) _ty ->
            fromIntegral (readInt32 block (fieldOffset_ schema lenField block))
          ByteString (Variable lenField)     ->
            fromIntegral (readInt32 block (fieldOffset_ schema lenField block))

instance {-# OVERLAPPABLE #-} (Selector s, Decode a) => Decode1 (M1 S s (K1 i a)) where
  decode1 schema accessor block =
    let
      m :: M1 i s f a
      m = undefined
    in
      M1 (K1 (decodeAccessor schema (accessor :. Field (fromString (selName m))) block))

instance (Decode1 f, Decode1 g) => Decode1 (f :*: g) where
  decode1 schema accessor block = decode1 schema accessor block :*: decode1 schema accessor block

------------------------------------------------------------------------

genericEncode :: (Generic a, Encode1 (Rep a)) => Schema -> Accessor -> a -> Block b -> IO ()
genericEncode schema accessor x block = encode1 schema accessor (from x) block

class Encode1 f where
  encode1 :: Schema -> Accessor -> f p -> Block b -> IO ()

instance Encode1 f => Encode1 (M1 D c f) where
  encode1 schema accessor (M1 x) block = encode1 schema accessor x block

instance Encode1 f => Encode1 (M1 C c f) where
  encode1 schema accessor (M1 x) block = encode1 schema accessor x block

instance (Selector s, Encode a, Show a) => Encode1 (M1 S s (K1 i [a])) where
  encode1 schema accessor (M1 (K1 xs)) block =
    let
      m :: M1 i s f a
      m = undefined
    in
      encodeArray schema (accessor :. Field (fromString (selName m))) xs block
    where
      encodeArray :: Encode a => Schema -> Accessor -> [a] -> Block b -> IO ()
      encodeArray schema accessor xs block = go 0 xs
        where
          go _i []       = return ()
          go  i (x : xs) = do
            encode schema (accessor :. Index i) x block
            go (i + 1) xs

instance {-# OVERLAPPABLE #-} (Selector s, Encode a, Show a) => Encode1 (M1 S s (K1 i a)) where
  encode1 schema accessor (M1 (K1 x)) block =
    let
      m :: M1 i s f a
      m = undefined
    in
      encodeAccessor schema (accessor :. Field (fromString (selName m))) block x

instance (Encode1 f, Encode1 g) => Encode1 (f :*: g) where
  encode1 schema accessor (x :*: y) block = do
    encode1 schema accessor x block
    encode1 schema accessor y block

------------------------------------------------------------------------

instance Encode Int32 where
  encode schema accessor (I32# i32#) block =
    withBlockAddr block $ \addr# ->
      IO $ \s ->
        (# writeInt32OffAddr# (addr# `plusAddr#` offset#) 0# i32# s, () #)
    where
      I# offset# = accessorOffset (Record schema) accessor block

instance Encode ByteString where
  encode schema accessor bs block =
    -- This is safe because we are not changing `src`.
    BS.unsafeUseAsCStringLen bs $ \(src, len) ->
     copyBytes (blockPtr block `plusPtr` offset) (castPtr src) len
    where
      offset = accessorOffset (Record schema) accessor block

instance Decode Int32 where
  decode schema accessor block = withBlockAddr block $ \addr# ->
    I32# (indexInt32OffAddr# (addr# `plusAddr#` offset#) 0#)
    where
      I# offset# = accessorOffset (Record schema) accessor block

instance Decode ByteString where
  decode schema accessor block =
    BS.unsafeCreate len (\dst -> copyBytes dst (castPtr src) len)
    where
      src = blockPtr block `plusPtr` offset
      len = case lookupAccessorType (Record schema) accessor of
              ByteString (Fixed len) -> len
              ByteString (Variable lenField) -> fromIntegral (readInt32 block (fieldOffset_ schema lenField block))
              ByteString NullTerminated -> undefined
              Magic bs -> BS.length bs
              ty -> error (show ty)
      offset = accessorOffset (Record schema) accessor block

------------------------------------------------------------------------

verifyMagic :: Schema -> Block a -> IO Bool
verifyMagic schema block = go True (Map.toList (schemaTypes schema))
  where
    go False _                          = return False
    go _acc []                          = return True
    go acc ((field, Magic magic) : fts) = do
      let bs = decodeAccessor schema (Field field) block
      go (bs == magic && acc) fts
    go acc ((_field, _type) : fts) = go acc fts

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

decodeField :: Schema -> Field -> Block a -> Either String Value
decodeField schema field block =
  case lookupFieldType field schema of
    Nothing -> Left ("WrongField: " ++ show field)
    Just (ByteString _) -> return (ByteStringV (decodeAccessor schema (Field field) block))

encodeField :: Schema -> Field -> Block a -> Value -> IO ()
encodeField schema field block (ByteStringV bs) = encodeAccessor schema (Field field) block bs

encodeAccessor :: Encode a => Schema -> Accessor -> Block b -> a -> IO ()
encodeAccessor schema accessor block x =
  case typeCheckAccessor schema accessor of
    Left err -> error "EncodeTypeError err"
    Right () -> case boundCheck schema accessor block of
      Left err -> error "EncodeBoundCheck err"
      Right () -> encode schema accessor x block

decodeAccessor :: Decode a => Schema -> Accessor -> Block b -> a
decodeAccessor schema accessor block =
  case typeCheckAccessor schema accessor of
    Left err -> error "DecodeTypeError err"
    Right () -> case boundCheck schema accessor block of
      Left err -> error "BoundCheck err"
      Right () -> decode schema accessor block
