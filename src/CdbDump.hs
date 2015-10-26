module Main where

import HsTinyCDB

import System.Environment

usage = putStrLn "hs-cdbdump FILENAME"

main = do
  args <- getArgs
  if length args /= 1
  then usage
  else do
    let fileName = args !! 0
    useCdb fileName $ \cdb -> dumpCdb cdb
