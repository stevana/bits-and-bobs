{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.String
import Data.ByteString (ByteString)
import System.Environment
import System.Exit
import System.IO

import BitsAndBobs.Block
import BitsAndBobs.Schema

------------------------------------------------------------------------

-- https://git.savannah.nongnu.org/cgit/poke.git/tree/pickles/id3v1.pk
id3v1Schema :: Schema
id3v1Schema = newSchema
  [ ("audio",   Binary)
  , ("header",  Magic "TAG")
  , ("title",   ByteString (Fixed 30))
  , ("artist",  ByteString (Fixed 30))
  , ("album",   ByteString (Fixed 30))
  , ("year",    ByteString (Fixed 4))
  , ("comment", ByteString (Fixed 30))
  , ("genre",   UInt8)
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fp] -> do
      block <- mmapFile fp
      b <- verifyMagic id3v1Schema block
      if not b
        then do
          putStrLn "Couldn't find magic bytes, doesn't look like a mp3 file."
          exitFailure
        else do
          hSetBuffering stdout NoBuffering
          go block
    _otherwise -> do
      putStrLn "Usage: first argument must be a filepath to an mp3 file"
      exitFailure
  where
    go block = do
      putStr "mp3> "
      l <- getLine
      case words l of
        ["help"]   -> putStrLn "schema | read <field> | write <field> <value> | list | q(uit)" >> go block
        ["schema"] -> print (schemaTypes id3v1Schema) >> go block
        ["read", field] ->
            case decodeField' id3v1Schema (fromString field) block of
              Right v -> print v >> go block
              Left err -> putStrLn ("decode error: " ++ show err) >> go block
        ["write", field, value] ->
          case readValue id3v1Schema (fromString field) value of
            Left err -> putStrLn ("read error: " ++ err) >> go block
            Right value' -> do
              r <- encodeField' id3v1Schema (fromString field) block value'
              case r of
                Right () -> go block
                Left err -> putStrLn ("encode error: " ++ show err) >> go block
        ["list"] -> do
          print $ Id3V1 <$> decodeField id3v1Schema "title" block
                        <*> decodeField id3v1Schema "artist" block
                        <*> decodeField id3v1Schema "album" block
                        <*> decodeField id3v1Schema "year" block
                        <*> decodeField id3v1Schema "comment" block
          go block
        ["q"]      -> exitSuccess
        ["quit"]   -> exitSuccess
        _otherwise -> putStrLn "invalid command" >> go block

data Id3V1 = Id3V1
  { title   :: ByteString
  , artist  :: ByteString
  , album   :: ByteString
  , year    :: ByteString
  , comment :: ByteString
  }
  deriving Show


{-
data Genre =
  ["blues", "classic rock", "country", "dance", "disco", "funk",
   "grunge", "hip-hop", "jazz", "metal", "new age", "oldies",
   "other", "pop", "rhythm and blues", "rap", "reggae", "rock",
   "techno", "industrial", "alternative", "ska", "death metal",
   "pranks", "soundtrack", "euro-techno", "ambient", "trip-hop",
   "vocal", "jazz and funk", "fusion",  "trance", "classical",
   "instrumental", "acid", "house", "game", "sound clip", "gospel",
   "noise", "alternative rock", "bass", "soul", "punk", "space",
   "meditative", "instrumental pop", "instrumental rock", "ethnic",
   "gothic", "darkwave", "techno-industrial", "electronic",
   "pop-folk", "eurodance", "dream", "southern rock", "comedy",
   "cult", "gangsta", "top 40", "christian rap", "pop/funk",
   "jungle music", "native US", "cabaret", "new wave",
   "psychedelic", "rave",   "showtunes", "trailer", "lo-fi",
   "tribal", "acid punk", "acid jazz", "polka", "retro",
   "musical", "rock n roll", "hard rock",

-}
