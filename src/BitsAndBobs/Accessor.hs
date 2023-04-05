{-# LANGUAGE OverloadedStrings #-}

module BitsAndBobs.Accessor (module BitsAndBobs.Accessor) where

import BitsAndBobs.Schema

------------------------------------------------------------------------

infixl 9 :.
data Accessor = Field Field | Accessor :. Accessor | Index Int

data AccessorTypeError = TE

typeCheckAccessor :: Schema -> Accessor -> Either AccessorTypeError ()
typeCheckAccessor = undefined

data OutOfBoundsError = OOB

boundCheck :: Schema -> Accessor -> Maybe a -> Either OutOfBoundsError ()
boundCheck = undefined

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

------------------------------------------------------------------------

exampleNestedSchema :: Schema
exampleNestedSchema = newSchema
  [ ("length", Int32)
  , ("array", Array (Fixed 10) (Record (newSchema [ ("foo", Int32)
                                                  , ("bar", Int32)
                                                  ])))
  ]

unit_accessorOffset :: Bool
unit_accessorOffset =
  16 == accessorOffset (Record exampleNestedSchema) (Field "array" :. Index 1 :. Field "bar")
-- length = 4 + array[0] = 8 + array[1].foo = 4 == 16

exampleNestedSchemaFooter :: Schema
exampleNestedSchemaFooter = newSchema
  [ ("binary", Binary)
  , ("array", Array (Fixed 10) (Record (newSchema [ ("foo", Int32)
                                                  , ("bar", Int32)
                                                  ])))
  , ("int", Int32)
  ]

unit_accessorOffsetFooter :: Bool
unit_accessorOffsetFooter = -72 ==
  accessorOffset (Record exampleNestedSchemaFooter) (Field "array" :. Index 1 :. Field "bar")
  -- array starts at -4 - (10 * 8) + array[0] = 8 + array[1].foo = 4, so -80 + 8 + 4 == -72
