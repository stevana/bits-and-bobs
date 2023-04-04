{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}

module BitsAndBobs.Schema (module BitsAndBobs.Schema) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Control.Applicative
import Control.Exception
import Control.Monad.Except
import Data.ByteString (ByteString)
import Data.Monoid
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.String
import Foreign
import GHC.Exts
import GHC.Int
import Text.Read

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

verifyMagic :: Schema -> Block a -> IO Bool
verifyMagic schema block = go True (Map.toList (schemaTypes schema))
  where
    go False _                          = return False
    go _acc []                          = return True
    go acc ((field, Magic magic) : fts) = do
      let eBs = decodeField schema field block
      case eBs of
        Left _decodeError -> return False
        Right bs          -> go (bs == magic && acc)fts
    go acc ((_field, _type) : fts) = go acc fts

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

data DecodeError
  = WrongField Field
  | WrongType Type
  | WrongLengthType Type
  | WrongLengthField Field
  | FieldUnknownOffset Field
  | LengthFieldUnknownOffset Field
  deriving stock (Eq, Show)

data Value = ByteStringV ByteString
  deriving Show

prettyValue :: Value -> Text
prettyValue (ByteStringV bs) = Text.decodeUtf8Lenient bs

decodeField' :: Schema -> Field -> Block a -> Either DecodeError Value
decodeField' schema field block =
  case lookupFieldType field schema of
    Nothing -> Left undefined
    Just (ByteString _) -> ByteStringV <$> decodeField schema field block

encodeField' :: Schema -> Field -> Block a -> Value -> IO (Either EncodeError ())
encodeField' schema field block (ByteStringV bs) = encodeField schema field block bs

readValue :: Schema -> Field -> String -> Either String Value
readValue schema field string =
  case lookupFieldType field schema of
    Nothing -> Left "field isn't in schema"
    Just ty ->
      case ty of
        ByteString _ -> ByteStringV <$> readEither string

class Codec a where
  decodeField :: Schema -> Field -> Block b -> Either DecodeError a
  encodeField :: Schema -> Field -> Block b -> a -> IO (Either EncodeError ())

instance Codec Int32 where
  decodeField schema field block =
    helper (\offset# -> I32# (indexInt32OffAddr# (addr# `plusAddr#` offset#) 0#)) -- XXX: negative offset = read from back of file
    where
    !(Ptr addr#) = blockPtr block
    helper f =
      case lookupFieldType field schema of
        Nothing    -> Left (WrongField field)
        Just Int32 ->
          case fieldOffset schema field of
            Nothing -> Left (FieldUnknownOffset field)
            Just (I# offset#) -> Right (f offset#)
        Just t     -> Left (WrongType t)
  encodeField = undefined

instance Codec ByteString where
  decodeField schema field block =
    case lookupFieldType field schema of
      Nothing -> Left (WrongField field)
      Just (Magic magic) ->
        let len = BS.length magic in
        case fieldOffset schema field of
          Nothing -> undefined
          Just offset@(I# offset#) | offset >= 0 ->
            Right (BS.unsafeCreate len (\ptr -> copyBytes ptr (Ptr (addr# `plusAddr#` offset#)) len))
                                   | otherwise -> do
            let blockLen# = case fromIntegral (blockLength block) of
                              I# len# -> len#
            Right (BS.unsafeCreate len (\ptr -> copyBytes ptr (Ptr (addr# `plusAddr#` blockLen# `plusAddr#` offset#)) len))

      Just (ByteString (Fixed len)) ->
        case fieldOffset schema field of
          Nothing -> undefined
          Just offset@(I# offset#) | offset >= 0 ->
            Right (BS.unsafeCreate len (\ptr -> copyBytes ptr (Ptr (addr# `plusAddr#` offset#)) len))
                                   | otherwise -> do
            let blockLen# = case fromIntegral (blockLength block) of
                              I# len# -> len#
            Right (BS8.takeWhile (/= '\NUL')
                   (BS.unsafeCreate len (\ptr -> copyBytes ptr (Ptr (addr# `plusAddr#` blockLen# `plusAddr#` offset#)) len)))

      Just (ByteString (Variable lengthField)) ->
        case lookupFieldType lengthField schema of
          Nothing -> Left (WrongLengthField lengthField)
          Just Int32 ->
            case fieldOffset schema lengthField of
              Nothing -> Left (LengthFieldUnknownOffset lengthField)
              Just offset@(I# offset#) | offset >= 0 -> do
                let lengthAddr# = addr# `plusAddr#` offset#
                    len = fromIntegral (I32# (indexInt32OffAddr# lengthAddr# 0#))
                Right (BS.unsafeCreate len (\ptr -> copyBytes ptr (Ptr (addr# `plusAddr#` 4#)) len))
                                           | otherwise -> do
                let blockLen = blockLength block
                undefined

          Just t -> Left (WrongLengthType t)
      Just t -> Left (WrongType t)
    where
    !(Ptr addr#) = blockPtr block
  encodeField schema field block bs = runExceptT $ do
    let ty = ByteString (Fixed (BS.length bs))
    () <- typeCheckField schema field ty         <?> EncodeTypeError
    () <- boundCheckField schema field (Just ty) <?> OutOfBoundsError
    let fieldOffset = fieldOffset_ schema field
        -- XXX: move into fieldOffset?
        offset | fieldOffset >= 0 = fieldOffset
               | otherwise        = fromIntegral (blockLength block) + fieldOffset
    liftIO (copyBytesFromBS (blockPtr block `plusPtr` offset) bs)

copyBytesFromBS :: Ptr a -> ByteString -> IO ()
copyBytesFromBS dest bs =
  -- This is safe because we are not changing `src`.
  BS.unsafeUseAsCStringLen bs $ \(src, len) ->
    copyBytes dest (castPtr src) len

(<?>) :: Monad m => Either l r -> (l -> l') -> ExceptT l' m r
Left  l <?>  f = throwError (f l)
Right r <?> _f = return r

data EncodeError = EncodeTypeError FieldTypeError | OutOfBoundsError FieldOutOfBoundsError
  deriving stock Show

data FieldTypeError = FTE
  deriving stock Show

typeCheckField :: Schema -> Field -> Type -> Either FieldTypeError ()
typeCheckField _ _ _ = return ()

data FieldOutOfBoundsError = FOOB
  deriving stock Show

boundCheckField :: Schema -> Field -> Maybe Type -> Either FieldOutOfBoundsError ()
boundCheckField _ _ _= return ()

exampleSchema :: Schema
exampleSchema = newSchema [("length", Int32), ("string", ByteString (Variable "length"))]

unit_decodeInt32 :: IO ()
unit_decodeInt32 = allocaBytes 4 $ \ptr -> do
  let i :: Int32
      i = 1234
  poke ptr i
  let j = decodeField exampleSchema "length" (Block ptr 4)
  assert (j == Right i) (return ())

unit_decodeBS :: IO ()
unit_decodeBS = allocaBytes (sizeOf (4 :: Int32) + length ("hello" :: String)) $ \ptr -> do
  let len :: Int32
      len = 5
  poke ptr len
  let hello = map BS.c2w "hello"
  pokeArray (ptr `plusPtr` sizeOf (4 :: Int32)) hello
  let bs = decodeField exampleSchema "string" (Block ptr 9)
  assert (bs == Right (BS.pack hello)) (return ())
