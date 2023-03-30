{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}

module BitsAndBobs.Schema (module BitsAndBobs.Schema) where

import Control.Exception
import Data.String
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import GHC.Int
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Foreign
import GHC.Exts

------------------------------------------------------------------------

newtype Schema = Schema (Map Field Type)

newtype Field = Field String
  deriving newtype (IsString, Eq, Ord)

data Type = Int32 | ByteString Field
  deriving stock Eq

exampleSchema :: Schema
exampleSchema = Schema (Map.fromList [("length", Int32), ("string", ByteString "length")])

fieldOffset :: Schema -> Field -> Maybe Int
fieldOffset (Schema schema) field = go (Just 0) (Map.toList schema)
  where
    go _acc [] = Nothing
    go acc ((field', type') : fts)
      | field == field' = acc
      | otherwise       = go ((+) <$> acc <*> sizeOfType type') fts

sizeOfType :: Type -> Maybe Int
sizeOfType Int32 = Just 4
sizeOfType (ByteString _lengthField) = Nothing

data DecodeError
  = WrongField Field
  | WrongType Type
  | WrongLengthType Type
  | WrongLengthField Field
  | FieldUnknownOffset Field
  | LengthFieldUnknownOffset Field
  deriving stock Eq

class Codec a where
  decodeField :: Schema -> Field -> Ptr b -> Either DecodeError a

instance Codec Int32 where
  decodeField (Schema schema) field (Ptr addr#) = case Map.lookup field schema of
    Nothing    -> Left (WrongField field)
    Just Int32 -> case fieldOffset (Schema schema) field of
      Nothing -> Left (FieldUnknownOffset field)
      Just (I# offset#) -> Right (I32# (indexInt32OffAddr# (addr# `plusAddr#` offset#) 0#))
    Just t     -> Left (WrongType t)

instance Codec ByteString where
  decodeField (Schema schema) field (Ptr addr#) =
    case Map.lookup field schema of
      Nothing -> Left (WrongField field)
      Just (ByteString lengthField) ->
        case Map.lookup lengthField schema of
          Nothing -> Left (WrongLengthField lengthField)
          Just Int32 ->
            case fieldOffset (Schema schema) lengthField of
              Nothing -> Left (LengthFieldUnknownOffset lengthField)
              Just (I# offset#) -> do
                let lengthAddr# = addr# `plusAddr#` offset#
                    len = fromIntegral (I32# (indexInt32OffAddr# lengthAddr# 0#))
                Right (BS.unsafeCreate len (\ptr -> copyBytes ptr (Ptr (addr# `plusAddr#` 4#)) len))
          Just t -> Left (WrongLengthType t)
      Just t -> Left (WrongType t)

unit_decodeInt32 :: IO ()
unit_decodeInt32 = allocaBytes 4 $ \ptr -> do
  let i :: Int32
      i = 1234
  poke ptr i
  let j = decodeField exampleSchema "length" ptr
  assert (j == Right i) (return ())

unit_decodeBS :: IO ()
unit_decodeBS = allocaBytes (sizeOf (4 :: Int32) + length ("hello" :: String)) $ \ptr -> do
  let len :: Int32
      len = 5
  poke ptr len
  let hello = map BS.c2w "hello"
  pokeArray (ptr `plusPtr` sizeOf (4 :: Int32)) hello
  let bs = decodeField exampleSchema "string" ptr
  assert (bs == Right (BS.pack hello)) (return ())
