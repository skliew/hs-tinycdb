{-# LANGUAGE OverloadedStrings #-}
module Main where

import HsTinyCDB ( makeCdb, useCdb, readCdb, addKeyValue, readAllCdb, dumpCdb, WriteCdb(WriteCdb) )
import Test.Hspec
import System.Posix.Files
import System.IO.Silently

insertToCdb = do
  addKeyValue "1" "2"
  addKeyValue "3" "4"
  addKeyValue "5" "6"
  addKeyValue "5" "700"

insertNothingToCdb :: WriteCdb IO ()
insertNothingToCdb = WriteCdb (\cdb -> return $ Right ())

testDumpCdb = do
  describe "dumpCdb" $ do
    it "should dump all the keys/values from a cdb file correctly" $ do
      useCdb "test.cdb" $ \cdb -> do
        (result, _) <- capture $ dumpCdb cdb
        result `shouldBe` "+1,1:1->2\n+1,1:3->4\n+1,1:5->6\n+1,3:5->700\n\n"
    it "should dump all the keys/values from an empty cdb file correctly" $ do
      useCdb "test2.cdb" $ \cdb -> do
        (result, _) <- capture $ dumpCdb cdb
        result `shouldBe` "\n"

testReadAll = do
  describe "readAllCdb" $ do
    it "should read all values of a key from a cdb file correctly" $ do
      useCdb "test.cdb" $ \cdb -> do
        result <- readAllCdb "5" cdb
        result `shouldBe` (Right ["6", "700"])

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
    it "should make a empty cdb file" $ do
      makeResult <- makeCdb "test2.cdb" insertNothingToCdb
      makeResult `shouldBe` (Right 0)

cleanUp = do
  removeLink "test.cdb"
  removeLink "test2.cdb"

main :: IO ()
main = do
  hspec $ do
    testMakeCdb
    testReadCdb
    testReadAll
    testDumpCdb
  cleanUp


