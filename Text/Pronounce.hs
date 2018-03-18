{-# LANGUAGE OverloadedStrings #-}

module Text.Pronounce 
    ( DictComp
    , EntryWord
    , Phones
    , Stress
    , phonesForEntry
    , stressesForEntry
    , stresses
    , syllableCount
    , rhymingPart
    , search
    , searchStresses
    , rhymes
    , dictAppend
    , (<||>)
    , liftD
    , runPronounce
    ) where

-- | A module for interpreting parsed CMU dict, modelled after Allison Parrish's
-- python library pronouncing

import Text.Pronounce.ParseDict
import Control.Monad.Reader
import Data.Char
import qualified Data.Text as T
import qualified Data.Map as Map

-- | We are using the Reader monad to perform computations in the context of the
-- CMU dictionary without having to pass it in or worry about initializing every time
type DictComp = Reader CMUdict 

-- | Convenient type aliases for transcription and entry
type EntryWord = T.Text
type Phones = T.Text
type Stress = T.Text

phonesForEntry :: EntryWord -> DictComp [Phones]
phonesForEntry = fmap concat . asks . Map.lookup
--phonesForEntry = fmap (concat . maybeToList) . asks . Map.lookup

stressesForEntry :: EntryWord -> DictComp [Stress]
stressesForEntry = liftD stresses . phonesForEntry 

stresses :: Phones -> Stress
stresses = T.filter isDigit

syllableCount :: Phones -> Int
syllableCount = T.length . stresses

-- | Finds the rhyming part of the given phones. NOTE: I don't like the current
-- implementation. It's kind of clunky - Fix it 
rhymingPart :: Phones -> Phones
rhymingPart = T.unwords . reverse . takeWhileInc (not . (`T.isInfixOf` "12") . T.singleton . T.last) . reverse . T.words
    where takeWhileInc _ [] = []
          takeWhileInc p (x:xs) = x : if p x then takeWhileInc p xs else []

{- TO DO: Generalize the pattern in these functions -}
search :: Phones -> DictComp [EntryWord]
search subphones = asks $ Map.keys . Map.filter (or . fmap (T.isInfixOf subphones)) 

searchStresses :: Stress -> DictComp [EntryWord]
searchStresses stressp = asks $ Map.keys . Map.filter (or . fmap ((== stressp) . stresses))

rhymes :: EntryWord -> DictComp [EntryWord]
rhymes word = do
    dict <- ask
    let rhymeE = runReader (rhymesForEntry word) dict
        res = Map.filter (\x -> or $ (==) <$> rhymeE <*> fmap rhymingPart x) dict
    return $ filter (/= word) $ Map.keys res
        where rhymesForEntry = (fmap rhymingPart <$>) . phonesForEntry


infixl 3 <||>
-- | Useful for nondeterministically combining several dictionary computations
dictAppend, (<||>) :: (Applicative f, Monoid a) => DictComp (f a) -> DictComp (f a) -> DictComp (f a)
dictAppend = ((<*>) . fmap ((<*>) . fmap mappend))
(<||>) = dictAppend

-- | Lift functions to act on elements within a functor in a dictionary
-- computation, such as a list of possible phones or stresses
liftD :: (Functor f) => (a -> b) -> DictComp (f a) -> DictComp (f b)
liftD = fmap . fmap

runPronounce :: DictComp a -> CMUdict -> a
runPronounce = runReader
