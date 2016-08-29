module CounterTop.WordCount() where

import qualified Data.Map.Strict as Map

type WordCount = Map.Map String Integer

wordCount :: WordCount
wordCount = Map.empty

processWord :: String -> WordCount -> WordCount
processWord word wc = Map.insertWith (+) word 1 wc


--countWords :: String -> WordCount
--countWords contents = foldr processWord empty .  words . lines 





