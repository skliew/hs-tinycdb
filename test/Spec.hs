{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Map (fromList)
import HsTinyCDB ( makeCdb, useCdb, readCdb, addKeyValue, readAllCdb )
import Test.Hspec
import System.Posix.Files

insertToCdb = do
  addKeyValue "1" "2"
  addKeyValue "3" "4"
  addKeyValue "5" "6"
  addKeyValue "5" "7"

testReadAll = do
  describe "readAllCdb" $ do
    it "should read a cdb file correctly" $ do
      useCdb "test.cdb" $ \cdb -> do
        result <- readAllCdb "5" cdb
        result `shouldBe` (Right ["6", "7"])

testReadCdb = do
  describe "readCdb" $ do
    it "should read all values of a key from a cdb file correctly" $ do
      useCdb "test.cdb" $ \cdb -> do
        result <- readCdb "3" cdb
        result `shouldBe` (Right (Just "4"))

testMakeCdb =
  describe "makeCdb" $ do
    it "should make a cdb file" $ do
      makeResult <- makeCdb "test.cdb" insertToCdb
      makeResult `shouldBe` (Right 0)

cleanUp = removeLink "test.cdb"

main :: IO ()
main = do
  hspec $ do
    testMakeCdb
    testReadCdb
    testReadAll
  cleanUp


