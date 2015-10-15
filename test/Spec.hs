{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Map (fromList)
import HsTinyCDB ( makeCdb, useCdb, readCdb, addKeyValue )
import Test.Hspec
import System.Posix.Files

insertToCdb = do
  addKeyValue "1" "2"
  addKeyValue "3" "4"

testReadCdb = do
  describe "readCdb" $ do
    it "should read a cdb file correctly" $ do
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
  cleanUp


