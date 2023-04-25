{-# LANGUAGE OverloadedStrings #-}

module BitsAndBobs.Accessor (module BitsAndBobs.Accessor) where

import Control.Applicative
import Foreign

import BitsAndBobs.Schema
import BitsAndBobs.Block

------------------------------------------------------------------------

infixl 9 :.
data Accessor = Id | Field Field | Accessor :. Accessor | Index Int
  deriving Show

data AccessorTypeError = TE

typeCheckAccessor :: Schema -> Accessor -> Either AccessorTypeError ()
typeCheckAccessor _ _ = return ()

data OutOfBoundsError = OOB

boundCheck :: Schema -> Accessor -> Block a -> Either OutOfBoundsError ()
boundCheck _ _ _ = return ()

lookupAccessorType :: Type -> Accessor -> Type
lookupAccessorType Int32 (Field _) = Int32
lookupAccessorType ty Id = ty
lookupAccessorType (Record schema) (Field field) = lookupFieldType_ schema field
lookupAccessorType (Array _size ty) (Index _inx) = ty
lookupAccessorType ty (Id :. accessor')   = lookupAccessorType ty accessor'
lookupAccessorType ty (accessor :. Id)    = lookupAccessorType ty accessor
lookupAccessorType ty (accessor :. accessor')    =
  let
    ty' = lookupAccessorType ty accessor
  in
    lookupAccessorType ty' accessor'
lookupAccessorType ty accessor = error $ "lookupAccessorType: impossible, should have been caught by typechecker: " ++ show (ty, accessor)

accessorOffset :: Type -> Accessor -> Block a -> Int
accessorOffset ty Id _block = 0
-- accessorOffset Int32 (Field _) _block = 4
accessorOffset (Record schema) (Field field) block = fieldOffset_ schema field block
accessorOffset ty (Id :. accessor') block = accessorOffset ty accessor' block
-- accessorOffset ty (accessor :. Id) block = accessorOffset ty accessor block
accessorOffset ty (accessor :. accessor') block =
  let
    ty' = lookupAccessorType ty accessor
    offset = accessorOffset ty accessor block
    offset' = accessorOffset ty' accessor' block
  in
    offset + offset'
accessorOffset (Array _size ty) (Index idx) block = idx * sizeOfType  ty
accessorOffset ty accessor _block = error $ "accessorOffset: impossible, should have been caught by typechecker: " ++ show (ty, accessor)

fieldOffset :: Schema -> Field -> Block a -> Maybe Int
fieldOffset schema field block
  =   go  0 (schemaToList schema)
  <|> go' 0 (schemaToReverseList schema)
  where
    go _acc [] = Nothing
    go acc ((field', ty) : fts)
      | field == field'     = Just acc
      | sizeOfType ty == -1 = Nothing
      | otherwise           = go (acc + sizeOfType ty) fts

    go' _acc [] = Nothing
    go' acc ((field', ty) : fts)
      | field == field'     = Just (negate (acc + sizeOfType ty))
      | sizeOfType ty == -1 = Nothing
      | otherwise           = go' (acc + sizeOfType ty) fts

fieldOffset_ :: Schema -> Field -> Block a -> Int
fieldOffset_ schema field block = case fieldOffset schema field block of
  Nothing -> error $ "fieldOffset_: Nothing, field: " ++ show field ++ ", schema: " ++ show schema
  Just i | i >= 0    -> i
         -- Negative offset means we want to start reading form the back of the
         -- block instead.
         | otherwise -> fromIntegral (blockLength block) + i
