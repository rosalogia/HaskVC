module Main where

import Diff

main :: IO ()
main = rawDiff "test-files/old" "test-files/new" >>= parseDiff >>= (putStrLn . show)
