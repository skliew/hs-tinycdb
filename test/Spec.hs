{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Map (fromList)
import HsTinyCDB ( makeCdb, useCdb, readCdb, addKeyValue, readAllCdb, dumpCdb )
import Test.Hspec
import System.Posix.Files
import Data.Text
import System.IO.Silently

insertToCdb = do
  addKeyValue "1" "2"
  addKeyValue "3" "4"
  addKeyValue "5" "6"
  addKeyValue "5" "7"

testDumpCdb = do
  describe "dumpCdb" $ do
    it "should dump all the keys/values from a cdb file correctly" $ do
      useCdb "test.cdb" $ \cdb -> do
        (result, _) <- capture $ dumpCdb cdb
        result `shouldBe` "+1,1:1->2\n+1,1:3->4\n+1,1:5->6\n+1,1:5->7\n"

testReadAll = do
  describe "readAllCdb" $ do
    it "should read all values of a key from a cdb file correctly" $ do
      useCdb "test.cdb" $ \cdb -> do
        result <- readAllCdb "5" cdb
        result `shouldBe` (Right ["6", "7"])

testReadCdb = do
  describe "readCdb" $ do
    it "should read a cdb file correctly" $ do
      useCdb "test.cdb" $ \cdb -> do
        result <- readCdb "3" cdb
        result `shouldBe` (Right "4")

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
    testDumpCdb
  cleanUp


