{-# LANGUAGE OverloadedStrings #-}

module BitsAndBobs.AccessorTest where

import Test.Tasty.HUnit

import BitsAndBobs.Schema
import BitsAndBobs.Accessor

------------------------------------------------------------------------

exampleNestedSchema :: Schema
exampleNestedSchema = newSchema
  [ ("length", Int32)
  , ("array", Array (Fixed 10) (Record (newSchema [ ("foo", Int32)
                                                  , ("bar", Int32)
                                                  ])))
  ]

unit_accessorOffset :: Assertion
unit_accessorOffset = 16 @=?
  accessorOffset (Record exampleNestedSchema) (Field "array" :. Index 1 :. Field "bar")
  -- length = 4 + array[0] = 8 + array[1].foo = 4 == 16

exampleNestedSchemaFooter :: Schema
exampleNestedSchemaFooter = newSchema
  [ ("binary", Binary)
  , ("array", Array (Fixed 10) (Record (newSchema [ ("foo", Int32)
                                                  , ("bar", Int32)
                                                  ])))
  , ("int", Int32)
  ]

unit_accessorOffsetFooter :: Assertion
unit_accessorOffsetFooter = -72 @=?
  accessorOffset (Record exampleNestedSchemaFooter) (Field "array" :. Index 1 :. Field "bar")
  -- array starts at -4 - (10 * 8) + array[0] = 8 + array[1].foo = 4, so -80 + 8 + 4 == -72
