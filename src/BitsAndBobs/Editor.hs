{-# LANGUAGE ScopedTypeVariables #-}

module BitsAndBobs.Editor (module BitsAndBobs.Editor) where

import Control.Monad
import Data.String
import Data.Text (Text)
import qualified Data.Text.IO as Text
import System.Exit
import System.IO

import BitsAndBobs.Block
import BitsAndBobs.Generic
import BitsAndBobs.Schema

------------------------------------------------------------------------

data Edit a = Edit Schema

edit :: forall a. (Show a, Decode a) => Edit a -> FilePath -> IO ()
edit (Edit schema) file = do
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
          putStr "mp3> "
          l <- getLine
          case words l of
            ["help"]   -> Text.putStrLn help >> loop
            ["schema"] -> Text.putStrLn (prettySchema schema) >> loop
            ["read", field] ->
              case decodeField' schema (fromString field) block of
                Right v -> Text.putStrLn (prettyValue v) >> loop
                Left err -> putStrLn ("decode error: " ++ show err) >> loop
            ["write", field, value] ->
              case readValue schema (fromString field) value of
                Left err -> putStrLn ("read error: " ++ err) >> loop
                Right value' -> do
                  r <- encodeField' schema (fromString field) block value'
                  case r of
                    Right () -> loop
                    Left err -> putStrLn ("encode error: " ++ show err) >> loop
            ["list"] -> do
              print (decode schema block :: Either DecodeError a)
              loop
            ["q"]      -> exitSuccess
            ["quit"]   -> exitSuccess
            _otherwise -> putStrLn "invalid command" >> loop
