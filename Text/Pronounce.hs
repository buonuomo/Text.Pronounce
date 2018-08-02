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
    , EntryWord
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
    -- * Some Helper Functions
    , dictAppend
    , (<||>)
    , liftD
    ) where

import Text.Pronounce.ParseDict
import Control.Monad.Reader
import Data.Char (isDigit)
import qualified Data.Text as T
import qualified Data.Map as Map

-- | We are using the Reader monad to perform computations in the context of the
-- CMU dictionary without having to pass it in or worry about initializing every time
type DictComp = Reader CMUdict 

-- | Convenient type aliases for the @Text@ string containing the stress patern of a word
type Stress = T.Text

-- | Look up the pronunciation (list of possible phones) of a word in the
-- dictionary
phonesForEntry :: EntryWord -> DictComp [Phones]
phonesForEntry = fmap concat . asks . Map.lookup

-- | Gives the stress pattern for a given word in the dictionary
stressesForEntry :: EntryWord -> DictComp [Stress]
stressesForEntry = liftD stresses . phonesForEntry 

-- | Isolates the stress pattern from a sequence of phones
stresses :: Phones -> Stress
stresses = T.filter isDigit

-- | Gives the syllable count of a given pronunciation
syllableCount :: Phones -> Int
syllableCount = T.length . stresses

-- | Finds the rhyming part of the given phones. 
rhymingPart :: Phones -> Phones
rhymingPart = T.unwords 
            . reverse 
            . takeWhileInc (not . (`T.isInfixOf` "12") . T.singleton . T.last) 
            . reverse 
            . T.words
    where takeWhileInc _ [] = []
          takeWhileInc p (x:xs) = x : if p x then takeWhileInc p xs else []

-- | Initializes a dictionary computation based on a selector function that
-- operates on an individual phones. It returns a @DictComp@ containing a @CMUdict@
-- of all the entries that have at least one value satisfying the predicate.
searchDictBy :: (Phones -> Bool) -> DictComp CMUdict
searchDictBy = asks . Map.filter . any

-- | Given a sequence of phones, find all words that contain that sequence of
-- phones
search :: Phones -> DictComp [EntryWord]
search = fmap Map.keys . searchDictBy . T.isInfixOf

-- | Given a stress pattern, find all words that satisfy that pattern
searchStresses :: Stress -> DictComp [EntryWord]
searchStresses = fmap Map.keys . searchDictBy . (==) . stresses

-- | Given a word, finds all other words that rhyme with it
rhymes :: EntryWord -> DictComp [EntryWord]
rhymes word = (\entryPart -> fmap (filter (/= word) . Map.keys) 
                           . return 
                           . Map.filter (or . ((==) <$> entryPart <*>) . fmap rhymingPart) 
                         =<< ask
              ) =<< (liftD rhymingPart . phonesForEntry $ word)
    
-- | Useful for nondeterministically combining several dictionary computations.
-- Generally, one would call @foldr1 (\<||\>)@ to get all the possible results of
-- mapping a @DictComp@ over a line of text (multiple words).
dictAppend, (<||>) :: (Applicative f, Monoid a) => DictComp (f a) -> DictComp (f a) -> DictComp (f a)
dictAppend = ((<*>) . fmap ((<*>) . fmap mappend))
infixl 3 <||>
(<||>) = dictAppend

-- | Lift functions to act on elements within a functor in a dictionary
-- computation, such as a list of possible phones or stresses
liftD :: (Functor f) => (a -> b) -> DictComp (f a) -> DictComp (f b)
liftD = fmap . fmap

-- | Get the value from a series of Dictionary Computations by supplying the
-- dictionary to the computation. This is just @runReader@.
runPronounce :: DictComp a -> CMUdict -> a
runPronounce = runReader
