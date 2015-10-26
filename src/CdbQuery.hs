{-# LANGUAGE OverloadedStrings #-}
module Main where

import HsTinyCDB

import System.Posix.Env.ByteString
import Data.ByteString.Char8 as BS ( putStrLn, unpack )

usage = BS.putStrLn "hs-cdbquery key FILENAME"

main = do
  args <- getArgs
  if length args /= 2
  then usage
  else do
    let fileName = args !! 0
        key = args !! 1
    useCdb (BS.unpack fileName) $ \cdb -> do
      result <- readAllCdb key cdb
      case result of
        Right values -> mapM_ BS.putStrLn values
        Left errMsg -> BS.putStrLn errMsg
