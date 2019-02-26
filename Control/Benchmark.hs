module Control.Benchmark where

import Data.Time.Clock
import Control.DeepSeq

now :: IO DiffTime
now = do
    t <- getCurrentTime
    return (utctDayTime t)

benchmark :: NFData a => a -> IO DiffTime
benchmark fn = do
    start <- now
    end <- deepseq fn now
    return $ end - start
