{-# LANGUAGE OverloadedStrings #-}

module BitsAndBobs.Accessor (module BitsAndBobs.Accessor) where

import Foreign

import BitsAndBobs.Schema
import BitsAndBobs.Block

------------------------------------------------------------------------

infixl 9 :.
data Accessor = Field Field | Accessor :. Accessor | Index Int

data AccessorTypeError = TE

typeCheckAccessor :: Schema -> Accessor -> Either AccessorTypeError ()
typeCheckAccessor _ _ = return ()

data OutOfBoundsError = OOB

boundCheck :: Schema -> Accessor -> Block a -> Either OutOfBoundsError ()
boundCheck _ _ _ = return ()

accessorOffset :: Type -> Accessor -> Int
accessorOffset (Record schema) (Field field) = fieldOffset_ schema field
accessorOffset ty (accessor :. accessor') =
  let
    ty' = lookupAccessorType ty accessor
  in
    accessorOffset ty accessor + accessorOffset ty' accessor'
accessorOffset (Array _size ty) (Index idx) = idx * sizeOfType_ ty
accessorOffset _ _ = error "accessorOffset: impossible, should have been caught by typechecker"

lookupAccessorType :: Type -> Accessor -> Type
lookupAccessorType (Record schema) (Field field) = lookupFieldType_ schema field
lookupAccessorType (Array _size ty) (Index _inx) = ty
lookupAccessorType ty (accessor :. accessor')    =
  let
    ty' = lookupAccessorType ty accessor
  in
    lookupAccessorType ty' accessor'
lookupAccessorType _ _ = error "lookupAccessorType: impossible, should have been caught by typechecker"
