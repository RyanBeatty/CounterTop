{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module CounterTop.WordCount(countWords, WordCount) where

import qualified Data.Map.Strict as Map
import Data.Monoid

-- Type Synonym for a map from Strings to Integers
type WordCount = Map.Map String Integer

instance Monoid WordCount where
    -- The identity element is an empty WordCount map
    mempty = wordCount

    -- The combintion operation combines the counts
    -- of all same keys between both maps
    mappend = Map.unionWith (+) 

-- Simple Constructor for WordCount
-- Creates an empty map
wordCount :: WordCount
wordCount = Map.empty

-- Adds a word to the word count map if it doesn't exist
-- and increments occurence if it does
processWord :: String -> WordCount -> WordCount
processWord word wc = Map.insertWith (+) word 1 wc

-- Counts all word occurences in a line
proccessLine :: String -> WordCount -> WordCount
proccessLine line wc = foldr (processWord) wc . words $ line

-- Counts all words in a text chunk
proccessContents :: String -> WordCount
proccessContents = foldr (proccessLine) wordCount . lines

-- simple name binding for proccessContents
countWords :: String -> WordCount
countWords = proccessContents

