{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Map (fromList)
import HsTinyCDB ( makeCdb, useCdb, readCdb )
import System.Posix.Files

-- dict = fromList [("何", "これ"), ("生姜", "ない")]
dict = fromList [("1", "2"), ("3", "4")]

readCdbTest cdb = do
  value <- readCdb cdb "3"
  case value of
    Just a -> putStrLn "\nOK"
    _ -> error "NG"
  return ()

testCdb fileName = do
  result <- useCdb fileName (\cdb -> readCdbTest cdb)
  return result

main :: IO ()
main = do
  makeCdb "test.cdb" dict
  testCdb "test.cdb"
  removeLink "test.cdb"
  return ()

