{-# LANGUAGE OverloadedStrings #-}

module ParseDict where

-- | A module for parsing the CMU dict

import System.FilePath
import System.Directory
import Text.ParserCombinators.ReadP
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as Map

type CMUdict = Map.Map T.Text [T.Text]

-- | Initializes the cmu pronunctiation dictionary into our program, given an
-- optional file name of the dictionary
initDict :: Maybe FilePath -> IO CMUdict
initDict path = case path of 
                  Just p -> do
                      dict <- T.readFile p
                      return $ parseDict dict
                  Nothing -> do
                      pwd <- getCurrentDirectory
                      let dictPath = takeDirectory pwd </> "dict" </> "cmudict-0.7b.txt"
                      dict <- T.readFile dictPath
                      --mapM_ T.putStrLn $ filter ((==[]) . readP_to_S parseLine . T.unpack) . filter ((/=';') . T.head) . T.lines $ dict
                      return $ parseDict dict
                      
-- | Go through all the entries in the dictionary, parsing, and inserting into
-- the map data structure
parseDict :: T.Text-> CMUdict
parseDict dict = Map.fromList entries
    where entries = foldr parseInsert [("",[])] . filter ((/= ';') . T.head) . T.lines $ dict
          parseInsert line acc@((k,v):kvs) = let (k',v') = packAndParse line in 
                                                 if k == k' 
                                                    then (k,v':v):kvs
                                                    else (k',[v']):acc
          --packAndParse = (\(x:y:xs) -> (x,y)) . T.splitOn "  "
          packAndParse = (\(a,b) -> (T.pack a, T.pack b)) . fst . head . readP_to_S parseLine . T.unpack

-- Parses a line in the dictionary, returning as (key,val) pair, ignoring
-- parenthetical part if it exists
parseLine :: ReadP (String, String)
parseLine = (,) <$> (many get) <* (paren <++ string "") <* string "  "
                <*> (munch . const $ True)

paren :: ReadP String
paren = char '(' *> munch isDigit <* char ')'
