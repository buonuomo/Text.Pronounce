{-# LANGUAGE OverloadedStrings #-}

module Text.Pronounce.ParseDict 
    ( CMUdict
    , initDict
    ) where

-- | A module for parsing the CMU Pronouncing Dictionary

import Paths_pronounce
import System.FilePath
import Text.ParserCombinators.ReadP
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as Map

-- | A Map from Entries to lists of possible pronunciations, serving as our
-- representation of the CMU Pronouncing Dictionary
type CMUdict = Map.Map T.Text [T.Text]

-- | Initializes the cmu pronunctiation dictionary into our program, given an
-- optional file name of the dictionary
initDict :: Maybe FilePath -> IO CMUdict
initDict path = case path of 
                  Just p -> do
                      dict <- T.readFile p
                      return $ parseDict dict
                  Nothing -> do
                      dictPath <- getDataFileName "cmudict-0.7b"
                      dict <- T.readFile dictPath
                      return $ parseDict dict
                      
-- | Go through all the entries in the dictionary, parsing, and inserting into
-- the map data structure
parseDict :: T.Text-> CMUdict
parseDict = Map.fromListWith (++) . map packAndParse . filter ((/= ';') . T.head) . T.lines
    where packAndParse = (\(a,b) -> (T.pack a, [T.pack b])) . fst . head . readP_to_S parseLine . T.unpack

-- Parses a line in the dictionary, returning as (key,val) pair, ignoring
-- parenthetical part if it exists
parseLine :: ReadP (String, String)
parseLine = (,) <$> (many get) <* (paren <++ string "") <* string "  "
                <*> (munch . const $ True)

-- Helper function to parse numbers in between parentheses
paren :: ReadP String
paren = char '(' *> munch isDigit <* char ')'
