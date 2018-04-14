{- A program that parses the CMUdict and then encodes it into a binary file of
    the given name
-}

module Main where

import Text.Pronounce.ParseDict
import System.Environment (getArgs)
import Data.Binary
import Data.Text.Encoding
import qualified Data.Map as Map

-- | Given optional dictionary source and optional destination name, parse cmu
-- dict and write out encoded binary file
main :: IO ()
main = do
    arg <- getArgs
    case arg of 
      [] -> 
          encodeFile "cmubin" . Map.mapKeys encodeUtf8 . fmap (map encodeUtf8) =<< initDict Nothing False
      [dest] -> 
          encodeFile dest . Map.mapKeys encodeUtf8 . fmap (map encodeUtf8) =<< initDict Nothing False
      [src,dest] ->
          encodeFile dest . Map.mapKeys encodeUtf8 . fmap (map encodeUtf8) =<< initDict (Just src) False
      _ -> putStrLn "Usage: encodeDict [src] [dest]"


