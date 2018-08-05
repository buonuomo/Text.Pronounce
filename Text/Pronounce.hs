{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Text.Pronounce
Description : A library for interfacing with the CMU Pronouncing Dictionary
Copyright   : (c) Noah Goodman, 2018
License     : BSD3
Stability   : experimental

This is a library for interpresting the parsed Carnegie Mellon University Pronouncing
Dictionary. It is modelled after Allison Parrish's python library, @pronouncing@.
-}
module Text.Pronounce (
    -- * Datatypes
      CMUdict
    , DictComp
    , Entry
    , Phones
    , Stress
    -- * Using Text.Pronounce
    , DictSource
    , initDict
    , stdDict
    , runPronounce
    -- * Basic Functions
    , phonesForEntry
    , stressesForEntry
    , stresses
    , syllableCount
    -- * Searching the Dictionary
    , searchDictBy
    , search
    , searchStresses
    -- * Rhyming
    , rhymingPart
    , rhymes
    ) where

import           Text.Pronounce.ParseDict

import           Control.Monad.Trans.Reader
import           Control.Monad
import           Data.Char (isDigit)
import           Data.List (isInfixOf)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import qualified Data.Text as T
import           Safe (readMay)

-- | We are using the Reader monad to perform computations in the context of the
-- CMU dictionary without having to pass it in or worry about initializing every time
type DictComp = ReaderT CMUdict []

-- | Get the value from a series of Dictionary Computations by supplying the
-- dictionary to the computation. This is just @runReader@.
runPronounce :: DictComp a -> CMUdict -> [a]
runPronounce = runReaderT

-- | Convenient type aliases for the @Text@ string containing the stress patern of a word
type Stress = [Int]

-- | Look up the pronunciation (list of possible phones) of a word in the
-- dictionary
phonesForEntry :: Entry -> DictComp Phones
phonesForEntry word = mapReaderT join $ fmap concat (asks (Map.lookup word))

-- | Gives the stress pattern for a given word in the dictionary
stressesForEntry :: Entry -> DictComp Stress
stressesForEntry = fmap stresses . phonesForEntry

-- | Isolates the stress pattern from a sequence of phones
stresses :: Phones -> Stress
stresses = catMaybes . fmap (readMay . filter isDigit . T.unpack)

-- | Gives the syllable count of a given pronunciation
syllableCount :: Phones -> Int
syllableCount = length . stresses

-- | Finds the rhyming part of the given phones.
rhymingPart :: Phones -> Phones
rhymingPart = reverse
            . takeWhileInc (not . (`T.isInfixOf` "12") . T.singleton . T.last)
            . reverse
    where takeWhileInc _ [] = []
          takeWhileInc p (x:xs) = x : if p x then takeWhileInc p xs else []

-- | Initializes a dictionary computation based on a selector function that
-- operates on an individual phones. It returns a @DictComp@ containing a @CMUdict@
-- of all the entries that have at least one value satisfying the predicate.
searchDictBy :: (Phones -> Bool) -> DictComp CMUdict
searchDictBy = asks . Map.filter . any

-- | Given a sequence of phones, find all words that contain that sequence of
-- phones
search :: Phones -> DictComp Entry
search = mapReaderT join . fmap Map.keys . searchDictBy . isInfixOf

-- | Given a stress pattern, find all words that satisfy that pattern
searchStresses :: Stress -> DictComp Entry
searchStresses = mapReaderT join . fmap Map.keys . searchDictBy . flip ((==) . stresses)

-- | Given a word, finds all other words that rhyme with it
rhymes :: Entry -> DictComp Entry
rhymes word = do rhymeParts <- rhymingPart <$> phonesForEntry word
                 match <- mapReaderT join . fmap Map.keys . searchDictBy $ (\phone -> rhymingPart phone == rhymeParts)
                 guard (match /= word)
                 return match

