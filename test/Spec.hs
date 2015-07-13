{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Map (fromList)
import HsTinyCDB ( makeCdb )

-- dict = fromList [("何", "これ"), ("生姜", "ない")]
dict = fromList [("1", "2"), ("3", "4")]

main :: IO ()
main = do
  makeCdb "test.cdb" dict

