{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

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
    , noStress
    , stresses
    , syllableCount
    -- * Searching the Dictionary
    -- ** Field Selectors
    , entries
    , phones
    , pairs
    -- ** Refining a Search
    , DictField
    , filterDict
    , filterComp
    --, refineDict
    --, takeAll
    --, suchThat
    -- ** Specific searches
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
phonesForEntry word = ReaderT (concat . Map.lookup word)

-- | Gives the stress pattern for a given word in the dictionary
stressesForEntry :: Entry -> DictComp Stress
stressesForEntry = fmap stresses . phonesForEntry

-- | Strips the stress indicating numbers off of a phones
noStress :: Phones -> Phones
noStress = fmap (T.filter (not . isDigit))

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

class DictField a where
    filterDict :: (a -> Bool) -> CMUdict -> CMUdict

instance DictField Entry where
    filterDict = Map.filterWithKey . fmap const

instance DictField Phones where
    filterDict = Map.filter . any

instance DictField (Entry, Phones) where
    filterDict = Map.filterWithKey . fmap any . curry
    
-- | Initializes a dictionary computation based on a selector function that
-- operates on an individual phones. It returns a @DictComp@ containing a @CMUdict@
-- of all the entries that have at least one value satisfying the predicate.
filterComp :: DictField a => (a -> Bool) -> DictComp b -> DictComp b
filterComp = local . filterDict

-- | Syntactic sugar for the refineDict function, along with @suchThat@
--takeAll :: DictComp a -> () -> (Phones -> Bool) -> DictComp a
--takeAll selector _ refiner = refineDict selector refiner

-- | Merely a piece of syntactic sugar to make the takeAll function look nice
--suchThat :: ()
--suchThat = ()

-- | A @DictComp@ that simply returns a list of all the entry words in the cmu dict
entries :: DictComp Entry
entries = ReaderT Map.keys

-- | A Dictionary Computation that returns a list of all the lists of phones in
-- the @CMUdict@
phones :: DictComp [Phones]
phones = ReaderT Map.elems

-- | A Dictionary Computation that returns a list of all the @(key,value) pairs
-- in the CMU Dictionary
pairs :: DictComp (Entry,[Phones])
pairs = ReaderT Map.toList

-- | Given a sequence of phones, find all words that contain that sequence of
-- phones
search :: Phones -> DictComp Entry
search subPhones = filterComp (subPhones `isInfixOf`) entries

-- | Given a stress pattern, find all words that satisfy that pattern
searchStresses :: Stress -> DictComp Entry
searchStresses stress = filterComp ((== stress) . stresses) entries

-- | Given a word, finds all other words that rhyme with it
rhymes :: Entry -> DictComp Entry
rhymes word = do rhyme <- rhymingPart <$> phonesForEntry word
                 match <- filterComp ((== rhyme) . rhymingPart) entries
                 guard (match /= word)
                 return match
