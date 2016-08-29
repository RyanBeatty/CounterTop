import CounterTop.WordCount

import System.Environment (getArgs)
import System.Directory   (doesFileExist)

import Control.Monad

import Data.Monoid (mconcat)

main :: IO ()
main = do
    args <- getArgs
    filenames <- filterM doesFileExist args
    contents <- mapM readFile filenames
    let counts = map countWords contents
        result = mconcat counts
    print result
    







