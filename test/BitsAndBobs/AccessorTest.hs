{-# LANGUAGE OverloadedStrings #-}

module BitsAndBobs.AccessorTest where

import Test.Tasty.HUnit

import BitsAndBobs.Schema
import BitsAndBobs.Accessor
import BitsAndBobs.Codec

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
  accessorOffset (Record exampleNestedSchema) (Field "array" :. Index 1 :. Field "bar") undefined
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
  accessorOffset (Record exampleNestedSchemaFooter) (Field "array" :. Index 1 :. Field "bar") undefined
  -- array starts at -4 - (10 * 8) + array[0] = 8 + array[1].foo = 4, so -80 + 8 + 4 == -72

exampleNestedSchema' :: Schema
exampleNestedSchema' = newSchema
  [ ("int",    Int32)
  , ("record", Record $ newSchema [("l", Int32), ("r", Int32)])
  ]

unit_accessorOffset' :: Assertion
unit_accessorOffset' = 8 @=?
  accessorOffset (Record exampleNestedSchema') (Field "record" :. Field "r") undefined
  -- int = 4 + l = 4 == 8
