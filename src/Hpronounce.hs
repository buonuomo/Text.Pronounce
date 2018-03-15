{-# LANGUAGE OverloadedStrings #-}

module Hpronounce where

-- | A module for interpreting parsed CMU dict, modelled after Allison Parrish's
-- python library pronouncing

import Prelude hiding (filter,length,unwords,words,last)
import ParseDict
import Data.Maybe (maybeToList)
import Control.Monad.Reader
import Data.Char
import Data.Text as T hiding (concat,reverse,takeWhile,any)
import qualified Data.Map as Map

-- | We are using the Reader monad to perform computations in the context of the
-- CMU dictionary without having to pass it in or worry about initializing every time
type DictComp = Reader CMUdict

-- | Convenient type aliases for transcription and entry
type EntryWord = Text
type Phones = Text
type Stress = Text

phonesForEntry :: EntryWord -> DictComp [Phones]
phonesForEntry = fmap (concat . maybeToList) . asks . Map.lookup

stressesForEntry :: EntryWord -> DictComp [Stress]
stressesForEntry = fmap (fmap stresses) . phonesForEntry
-- stressesForEntry = asks . ((.) (fmap (fmap stresses)) . Map.lookup) 
-- stressesForEntry = fmap (fmap (fmap stresses)) phonesForEntry

stresses :: Phones -> Stress
stresses = filter isDigit

syllablesCount :: Phones -> Int
syllablesCount = length . stresses

-- | Finds the rhyming part of the given phones. NOTE: I don't like the current
-- implementation. It's kind of clunky - Fix it 
rhymingPart :: Phones -> Phones
rhymingPart = unwords . reverse . takeWhileInc (not . (`isInfixOf` "12") . singleton . last) . reverse . words
    where takeWhileInc _ [] = []
          takeWhileInc p (x:xs) = x : if p x then takeWhileInc p xs else []

search :: Phones -> DictComp [EntryWord]
search subphones = asks $ Map.keys . Map.filter (or . fmap (isInfixOf subphones)) 

searchStresses :: Stress -> DictComp [EntryWord]
searchStresses stressp = asks $ Map.keys . Map.filter (or . fmap ((== stressp) . stresses))

rhymes :: EntryWord -> DictComp [EntryWord]
rhymes = undefined

