{-# LANGUAGE OverloadedStrings #-}

module IambicLine where

-- | A test for the pronouncing API

import Text.Pronounce
import Text.Pronounce.ParseDict
import Control.Monad.Reader
import System.Console.Haskeline
import qualified Data.Text as T

-- | Test if a stress pattern is (loosely) in iambic meter
isIambic :: Stress -> Bool
isIambic "" = True
isIambic xs = ((`elem` ["22","11","21","01","02"]) . T.take 2 $ xs) && isIambic (T.drop 2 xs)

-- | Test if a line of text is iambic
isIambicLine :: T.Text -> DictComp Bool
isIambicLine = fmap (any isIambic)
             . foldr1 (<||>)
             . fmap stressesForEntry 
             . T.words

-- | A simple repl using haskeline that tells us whether or not what we type is
-- iambic
main :: IO ()
main = do 
    putStrLn "Type something, and I'll tell you if it's in iambic meter..."
    dict <- initDict Nothing 
    runInputT defaultSettings (loop dict)
        where
            loop :: CMUdict -> InputT IO ()
            loop cmu = do
                line <- getInputLine "# "
                case line of 
                    Nothing -> return ()
                    Just "" -> loop cmu
                    Just input -> do
                        outputStrLn . show . runPronounce (isIambicLine . T.toUpper . T.pack $ input) $ cmu
                        loop cmu
