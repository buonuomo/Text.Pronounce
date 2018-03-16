{-# LANGUAGE OverloadedStrings #-}

module Hpronounce where

-- | A module for interpreting parsed CMU dict, modelled after Allison Parrish's
-- python library pronouncing

import Prelude hiding (length,unwords,words,last)
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
--phoneForEntry word = lift $ Map.lookup
phonesForEntry = (concat <$>) . (asks . Map.lookup)
--phonesForEntry = fmap (concat . maybeToList) . asks . Map.lookup

stressesForEntry :: EntryWord -> DictComp [Stress]
stressesForEntry = (fmap stresses <$>) . phonesForEntry 
--stressesForEntry = fmap (fmap stresses) . phonesForEntry
-- stressesForEntry = asks . ((.) (fmap (fmap stresses)) . Map.lookup) 
-- stressesForEntry = fmap (fmap (fmap stresses)) phonesForEntry
stresses :: Phones -> Stress
stresses = T.filter isDigit

syllablesCount :: Phones -> Int
syllablesCount = length . stresses

-- | Finds the rhyming part of the given phones. NOTE: I don't like the current
-- implementation. It's kind of clunky - Fix it 
rhymingPart :: Phones -> Phones
rhymingPart = unwords . reverse . takeWhileInc (not . (`isInfixOf` "12") . singleton . last) . reverse . words
    where takeWhileInc _ [] = []
          takeWhileInc p (x:xs) = x : if p x then takeWhileInc p xs else []

{- TO DO: Generalize the pattern in these functions -}
search :: Phones -> DictComp [EntryWord]
search subphones = asks $ Map.keys . Map.filter (or . fmap (isInfixOf subphones)) 

searchStresses :: Stress -> DictComp [EntryWord]
searchStresses stressp = asks $ Map.keys . Map.filter (or . fmap ((== stressp) . stresses))

rhymes :: EntryWord -> DictComp [EntryWord]
rhymes word = do
    dict <- ask
    let rhymeE = runReader (rhymesForEntry word) dict
        res = Map.filter (\x -> or $ (==) <$> rhymeE <*> fmap rhymingPart x) dict
    return $ Prelude.filter (/= word) $ Map.keys res
        where rhymesForEntry = (fmap rhymingPart <$>) . phonesForEntry
--rhymes word = asks $ Map.keys . Map.filter (\x -> asks $ runReader $ fmap or (fmap (fmap ((== (rhymingPart x)) . rhymingPart)) (stressesForEntry word)))
{-
rhymes :: EntryWord -> DictComp [EntryWord]
rhymes word = asks $ \dict -> do 
    entryRhymes <- runReader (fmap (fmap rhymingPart) . phonesForEntry $ word) dict
    vals <- Map.elems dict
    vals' <- vals
    guard (rhymingPart entryRhymes == rhymingPart vals')
    return vals'
-}
