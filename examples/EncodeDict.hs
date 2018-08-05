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
          encodeFile "cmubin" =<< initDict Nothing PlainText
      [dest] -> 
          encodeFile dest =<< initDict Nothing PlainText
      [src,dest] ->
          encodeFile dest =<< initDict (Just src) PlainText
      _ -> putStrLn "Usage: encodeDict [src] [dest]"


