{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Text.Pronounce.ParseDict
Description : Module for parsing the CMU Dictionary
Copyright   : (c) Noah Goodman, 2018
License     : BSD3
Stability   : experimental

This module has functions for parsing the CMU pronouncing dictionary, and exports the 
@CMUdict@ type and the function @initDict@ to the main module "Text.Pronounce"
-}


module Text.Pronounce.ParseDict 
    ( Entry
    , Phones
    , CMUdict
    , DictSource(..)
    , initDict
    , stdDict
    , parseDict
    , parseLine
    ) where

import           Paths_pronounce

import           Control.Arrow ((***))
import           Data.Binary (Binary, decodeFile)
import           Data.Char
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text.Encoding
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.FilePath
import           Text.ParserCombinators.ReadP

-- | Represents an entry word in the cmu pronouncing dictionary (simply an alias
-- for @Text@ to improve type specificity and readability
type Entry = Text

-- | Represents a string containing the phonetic breakdown of a word, in a
-- similar manner to the @EntryWord@ type
type Phones = [Text]

-- | A Map from @Entry@s to lists of possible pronunciations (@Phones@), serving as our
-- representation of the CMU Pronouncing Dictionary
type CMUdict = Map Entry [Phones]

-- | Options for the initial source of the CMUDict. Currently, we can either
-- parse from plaintext file or load preprocessed binary
data DictSource = PlainText | Binary

-- | Initializes the cmu pronunctiation dictionary into our program, given an
-- optional file name of the dictionary
initDict :: Maybe FilePath -> DictSource -> IO CMUdict
initDict path dictSource = case dictSource of
    Binary ->
        case path of 
          Just p ->
              --return . Map.mapKeys decodeUtf8 . fmap (map decodeUtf8) =<< decodeFile p
              decodeFile p
          Nothing ->
              --return . Map.mapKeys decodeUtf8 . fmap (map decodeUtf8) =<< decodeFile =<< getDataFileName "cmubin"
              decodeFile =<< getDataFileName "cmubin"
    PlainText ->
        case path of 
          Just p -> 
              return . parseDict =<< T.readFile p
          Nothing -> 
              return . parseDict =<< T.readFile =<< getDataFileName "cmuutf"

-- | Default settings for @initDict@
stdDict :: IO CMUdict
stdDict = initDict Nothing Binary

-- | Go through all the entries in the dictionary, parsing, and inserting into
-- the map data structure
parseDict :: T.Text -> CMUdict
parseDict = Map.fromListWith (++) . map packAndParse . filter ((/= ';') . T.head) . T.lines
    where packAndParse = (T.pack *** (:[]) . T.words . T.pack) . fst . head . readP_to_S parseLine . T.unpack

-- | Parses a line in the dictionary, returning as @(key,val)@ pair, ignoring
-- parenthetical part if it exists
parseLine :: ReadP (String, String)
parseLine = (,) <$> (many get) <* (paren <++ string "") <* string "  "
                <*> (munch . const $ True)

-- Helper function to parse numbers in between parentheses
paren :: ReadP String
paren = char '(' *> munch isDigit <* char ')'
