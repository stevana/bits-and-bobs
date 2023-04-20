{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}

module BitsAndBobs.Schema (module BitsAndBobs.Schema) where

import Control.Applicative
import Control.Exception
import Control.Monad.Except
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Foreign
import GHC.Exts
import GHC.Int
import GHC.IO (IO(IO))

import BitsAndBobs.Block

------------------------------------------------------------------------

data Schema = Schema
  { schemaTypes      :: Map Field Type
  , schemaFieldOrder :: [Field]
  -- XXX: offsets?
  }
  deriving stock (Eq, Show)

newtype Field = MkField Text
  deriving newtype (IsString, Eq, Ord, Show)

data Type = Magic ByteString | Int32 | ByteString Size | UInt8 | Binary | Record Schema | Array Size Type
  deriving stock (Eq, Show)

data Size = Fixed Int | Variable Field | NullTerminated
  deriving stock (Eq, Show)

newSchema :: [(Field, Type)] -> Schema
newSchema fts = Schema (Map.fromList fts) (map fst fts)

lookupFieldType :: Field -> Schema -> Maybe Type
lookupFieldType field schema = Map.lookup field (schemaTypes schema)

lookupFieldType_ :: Schema -> Field -> Type
lookupFieldType_ schema field = schemaTypes schema Map.! field

schemaToList :: Schema -> [(Field, Type)]
schemaToList schema = go [] (schemaFieldOrder schema)
  where
    go acc []               = reverse acc
    go acc (field : fields) = go ((field, schemaTypes schema Map.! field) : acc) fields

schemaToReverseList :: Schema -> [(Field, Type)]
schemaToReverseList schema = go [] (schemaFieldOrder schema)
  where
    go acc []               = acc
    go acc (field : fields) = go ((field, schemaTypes schema Map.! field) : acc) fields

prettySchema :: Schema -> Text
prettySchema = go [] . schemaToList
  where
    go :: [Text] -> [(Field, Type)] -> Text
    go acc [] = Text.unlines (reverse acc)
    go acc ((MkField field, ty) : fts) = go ((field <> " : " <> prettyType ty) : acc) fts

prettyType :: Type -> Text
prettyType = Text.pack . show -- XXX

fieldOffset :: Schema -> Field -> Maybe Int
fieldOffset schema field
  =   go  (Just 0) (schemaToList schema)
  <|> go' (Just 0) (schemaToReverseList schema)
  where
    go _acc [] = Nothing
    go acc ((field', ty) : fts)
      | field == field' = acc
      | otherwise       = go ((+) <$> acc <*> sizeOfType ty) fts

    go' _acc [] = Nothing
    go' acc ((field', ty) : fts)
      | field == field' = fmap negate ((+) <$> acc <*> sizeOfType ty)
      | otherwise       = go' ((+) <$> acc <*> sizeOfType ty) fts

fieldOffset_ :: Schema -> Field -> Int
fieldOffset_ schema = fromJust . fieldOffset schema

sizeOfType :: Type -> Maybe Int
sizeOfType (Magic bs)                        = Just (BS.length bs)
sizeOfType UInt8                             = Just (sizeOf (1 :: Word8))
sizeOfType Int32                             = Just (sizeOf (4 :: Int32))
sizeOfType (ByteString (Fixed len))          = Just len
sizeOfType (Record schema)                   = fmap sum (sequence (map sizeOfType (map snd (schemaToReverseList schema))))
sizeOfType (Array (Fixed len) ty)            = (*) <$> pure len <*> sizeOfType ty
sizeOfType (ByteString (Variable _lenField)) = Nothing
sizeOfType Binary                            = Nothing
sizeOfType ty = error (show ty)

sizeOfType_ :: Type -> Int
sizeOfType_ = fromJust . sizeOfType

data EncodeError = EncodeTypeError FieldTypeError | OutOfBoundsError FieldOutOfBoundsError
  deriving stock Show

data FieldTypeError = FTE
  deriving stock Show

data FieldOutOfBoundsError = FOOB
  deriving stock Show
