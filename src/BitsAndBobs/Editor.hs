{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module BitsAndBobs.Editor (module BitsAndBobs.Editor) where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Exit
import System.IO

import BitsAndBobs.Accessor
import BitsAndBobs.Block
import BitsAndBobs.Codec
import BitsAndBobs.Schema

------------------------------------------------------------------------

data Edit a = Edit { editName :: Text, editSchema :: Schema }

edit :: forall a. (Show a, Decode a) => Edit a -> FilePath -> IO ()
edit (Edit name schema) file = do
  block <- mmapFile file

  b <- verifyMagic schema block
  when (not b) $ do
    Text.putStrLn "Failed to verify magic bytes, the file doesn't appear matches the schema."
    exitFailure

  hSetBuffering stdout NoBuffering
  go block
  where
    help :: Text
    help = "schema | read <field> | write <field> <value> | list | q(uit)"

    go block = loop
      where
        loop = do
          Text.putStr (name <> "> ")
          l <- Text.getLine
          case Text.words l of
            ["help"]   -> Text.putStrLn help >> loop
            ["schema"] -> Text.putStrLn (prettySchema schema) >> loop
            ["read", field] ->
              case decodeField schema (MkField field) block of
                Right v -> Text.putStrLn (prettyValue v) >> loop
                Left err -> putStrLn ("decode error: " ++ show err) >> loop
            ["write", field, value] ->
              case readValue schema (MkField field) value of
                Left err -> putStrLn ("read error: " ++ err) >> loop
                Right value' -> do
                  encodeField schema (MkField field) block value'
                  loop
            ["list"] -> do
              print (decode schema Id block :: a)
              loop
            ["q"]      -> exitSuccess
            ["quit"]   -> exitSuccess
            _otherwise -> Text.putStrLn "invalid command" >> loop
