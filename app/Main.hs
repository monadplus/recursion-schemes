{-# LANGUAGE TypeApplications #-}
module Main where

import Example (mergesort)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [arg1] -> do
        let n = read @Int arg1
        print $ mergesort [n, n-1..0]
    _ -> putStrLn "Usage: cabal run recursion -- length_of_array"
