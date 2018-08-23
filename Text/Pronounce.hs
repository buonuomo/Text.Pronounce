{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Text.Pronounce
Description : A library for interfacing with the CMU Pronouncing Dictionary
Copyright   : (c) Noah Goodman, 2018
License     : BSD3
Stability   : experimental

This is a library for interpreting the parsed Carnegie Mellon University Pronouncing
Dictionary. It is modelled after Allison Parrish's python library, @pronouncing@.
-}
module Text.Pronounce (
    -- * Fundamentals
    -- ** Basic Datatypes
      CMUdict
    , Entry
    , Phones
    , Stress
    -- ** The Dictionary Computation Monad
    , DictComp
    , dictcomp
    , runPronounce
    -- ** Using Text.Pronounce
    , initDict
    , stdDict
    , DictSource
    -- * Basic Functions
    , phonesForEntry
    , stressesForEntry
    , noStress
    , stresses
    , syllableCount
    -- * Searching the Dictionary
    -- ** Field Selectors
    , entries
    , phoneses
    , pairs
    -- ** Filtering Searches
    , DictField
    , filterDict
    , filterComp
    -- ** Specific Searches
    , search
    , searchStresses
    -- * Rhyming
    , rhymingPart
    , rhymesUsing
    , rhymes
    ) where

import           Text.Pronounce.ParseDict

import           Control.Monad.Reader
import           Control.Monad
import           Data.Char (isDigit)
import           Data.Function (on)
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import qualified Data.Text as T
import           Safe (readMay)

-- | We are using the List monad inside the ReaderT monad to perform nondeterministic computations
-- (due to the possibility of multiple @Phones@ patterns per @Entry@) in the context of the
-- CMU dictionary without having to pass it as an argument to every function.
type DictComp = ReaderT CMUdict []

-- | Contruct a Dictionary Computation based on a selector function on the
-- @CMUdict@ that returns a list of possible results. This is just a synonym for
-- the @ReaderT@ constructor.
dictcomp :: (CMUdict -> [a]) -> DictComp a
dictcomp = ReaderT

-- | Get the possible values resulting from a series of Dictionary Computations by supplying the
-- dictionary to the computation. This is just @runReaderT@.
runPronounce :: DictComp a -> CMUdict -> [a]
runPronounce = runReaderT

-- | Type alias for a stress pattern, which is a list of integers 0-2 indicating
-- stress.
--
--   * 0 -> unstressed
--   * 1 -> primary stress
--   * 2 -> secondary stress
type Stress = [Int]

-- | Look up the pronunciation (list of possible phones) of a word in the
-- dictionary
phonesForEntry :: Entry -> DictComp Phones
phonesForEntry = dictcomp . Map.findWithDefault []

-- | Gives the stress pattern for a given word in the dictionary
stressesForEntry :: Entry -> DictComp Stress
stressesForEntry = fmap stresses . phonesForEntry

-- | Strips the stress-indicating numbers off of a phones
noStress :: Phones -> Phones
noStress = fmap (T.filter (not . isDigit))

-- | Isolates the stress pattern from a sequence of phones
stresses :: Phones -> Stress
stresses = catMaybes . fmap (readMay . filter isDigit . T.unpack)

-- | Gives the syllable count of a given pronunciation
syllableCount :: Phones -> Int
syllableCount = length . stresses

-- | Finds the rhyming part of the given phones, where the rhyming part is
-- defined as everything in a word after and including the last stressed or
-- semistressed phone. Note that this is merely one
-- interpretation of what constitutes a rhyme. There exist both stricter and
-- looser definitions that may be suited to different purposes.
rhymingPart :: Phones -> Phones
rhymingPart = reverse
            . takeWhileInc ((`notElem` ['1','2']) . T.last)
            . reverse
    where takeWhileInc _ [] = []
          takeWhileInc p (x:xs) = x : if p x then takeWhileInc p xs else []

-- | A class that provides a generalized function @filterDict@ for filtering the
-- @CMUdict@ based on a choice of different "fields"
class DictField a where
    filterDict :: (a -> Bool) -> CMUdict -> CMUdict

instance DictField Entry where
    filterDict = Map.filterWithKey . fmap const

instance DictField Phones where
    filterDict = Map.filter . any

instance DictField (Entry, Phones) where
    filterDict = Map.filterWithKey . fmap any . curry

instance DictField [Phones] where
    filterDict = Map.filter

instance DictField (Entry, [Phones]) where
    filterDict = Map.filterWithKey . curry

-- | Filter a the results of a @DictComp@, taking only those whose corresponing
-- entries conform to the selector function
filterComp :: DictField a => (a -> Bool) -> DictComp b -> DictComp b
filterComp = local . filterDict

-- | A Dictionary Computation that returns a list of all the entry words in the
-- @CMUdict@
entries :: DictComp Entry
entries = dictcomp Map.keys

-- | A Dictionary Computation that returns a list of all the lists of phones in
-- the @CMUdict@
phoneses :: DictComp [Phones]
phoneses = dictcomp Map.elems

-- | A Dictionary Computation that returns a list of all the @(key,value) pairs
-- in the @CMUdict@
pairs :: DictComp (Entry,[Phones])
pairs = dictcomp Map.toList

-- | Given a sequence of phones, find all words that contain that sequence of
-- phones
search :: Phones -> DictComp Entry
search subPhones = filterComp (subPhones `isInfixOf`) entries

-- | Given a stress pattern, find all words that satisfy that pattern
searchStresses :: Stress -> DictComp Entry
searchStresses stress = filterComp ((== stress) . stresses) entries

-- | Given a function that tells whether or not two sets of phones rhyme, and an
-- entry, find all words that rhyme with that entry according to the provided
-- definition of a rhyme
rhymesUsing :: (Phones -> Phones -> Bool) -> Entry -> DictComp Entry
rhymesUsing rhymesWith word = do phones <- phonesForEntry word
                                 match <- filterComp (rhymesWith phones) entries
                                 guard (match /= word)
                                 return match

-- | Given a word, finds all other words that rhyme with it, using the default
-- @rhymingPart@ definition
rhymes :: Entry -> DictComp Entry
rhymes = rhymesUsing ((==) `on` rhymingPart)
