module CounterTop.WordCount(countWords, WordCount) where

import qualified Data.Map.Strict as Map
import Data.Monoid

-- Type Synonym for a map from Strings to Integers
data WordCount = WordCount {
    wordcount :: Map.Map String Integer
}

instance Monoid WordCount where
    -- The identity element is an empty WordCount map
    mempty = newWordCount

    -- The combintion operation combines the counts
    -- of all same keys between both maps
    mappend x y = wordCount $ Map.unionWith (+) (wordcount x) (wordcount y)

-- Simple Constructor for WordCount
wordCount :: Map.Map String Integer -> WordCount
wordCount map = WordCount map

-- Creates an empty WordCount map
newWordCount :: WordCount
newWordCount = wordCount Map.empty

-- Adds a word to the word count map if it doesn't exist
-- and increments occurence if it does
processWord :: String -> WordCount -> WordCount
processWord word wc = wordCount . Map.insertWith (+) word 1 . wordcount $ wc

-- Counts all word occurences in a line
proccessLine :: String -> WordCount -> WordCount
proccessLine line wc = foldr (processWord) wc . words $ line

-- Counts all words in a text chunk
proccessContents :: String -> WordCount
proccessContents = foldr (proccessLine) newWordCount . lines

-- simple name binding for proccessContents
countWords :: String -> WordCount
countWords = proccessContents

