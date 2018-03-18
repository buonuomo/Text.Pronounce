{-# LANGUAGE OverloadedStrings #-}

module IambicLine where

-- | A test for the pronouncing API

import Text.Pronounce
import Text.Pronounce.ParseDict
import Control.Monad.Reader
import qualified Data.Text as T

isIambicP :: Stress -> Bool
isIambicP "" = True
isIambicP xs = ((`elem` ["22","11","21","01","02"]) . T.take 2 $ xs) && isIambicP (T.drop 2 xs)

isIambicLine :: T.Text -> DictComp Bool
isIambicLine = fmap or 
             . fmap (fmap isIambicP)
             . foldr1 (<||>)
             . fmap stressesForEntry 
             . T.words

