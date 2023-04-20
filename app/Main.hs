{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.ByteString (ByteString)
import Data.String
import GHC.Generics
import System.Environment
import System.Exit
import System.IO

import BitsAndBobs.Editor
import BitsAndBobs.Codec
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
    [fp] -> edit (Edit "mp3" id3v1Schema :: Edit Id3V1) fp
    _otherwise -> do
      putStrLn "Usage: first argument must be a filepath to an mp3 file"
      exitFailure

data Id3V1 = Id3V1
  { title   :: ByteString
  , artist  :: ByteString
  , album   :: ByteString
  , year    :: ByteString
  , comment :: ByteString
  }
  deriving (Show, Generic)

instance Decode Id3V1


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
