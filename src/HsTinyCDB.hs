{-# LANGUAGE OverloadedStrings #-}
module HsTinyCDB where

import System.Posix.IO ( handleToFd, fdToHandle )
import System.Posix.Types ( Fd )
import System.Posix.Files ( rename )
import System.IO
import Data.Text as T
import Data.Text.IO as TIO
import Foreign.Marshal.Alloc
import Data.Text.Foreign
import Foreign.Storable
import Foreign.Ptr ( castPtr )
import Control.Applicative
import Control.Monad ( ap, liftM )
import Control.Monad.Error
import Data.Either ( either )
import Foreign.C.Types ( CUInt(CUInt) )
import Foreign.Ptr ( Ptr )

import TinyCDB

newtype WriteCdb m a = WriteCdb { runWriteCdb :: CDBMHandle -> m (Either Text a) }

instance Monad m => Monad (WriteCdb m) where
  return a = WriteCdb $ \cdbm -> return $ Right a
  x >>= f = WriteCdb $ \cdbm -> do
              value <- runWriteCdb x cdbm
              case value of
                Left error -> return $ Left error
                Right v -> runWriteCdb (f v) cdbm

instance Monad m => Functor (WriteCdb m) where
  fmap = liftM

instance Monad m => Applicative (WriteCdb m) where
  pure = return
  (<*>) = ap

instance Error Text where
  noMsg = ""
  strMsg str = pack str

fdFromFile :: FilePath -> IO System.Posix.Types.Fd
fdFromFile fileName = do
  handle <- openBinaryFile fileName ReadWriteMode
  fd <- handleToFd handle
  return fd

addKeyValue :: Text -> Text -> WriteCdb IO Int
addKeyValue k v = WriteCdb $ \cdbm -> do
  useAsPtr k $ \kPtr kLen -> do
    useAsPtr v $ \vPtr vLen -> do
      result <- cdb_make_add cdbm (castPtr kPtr) (fromIntegral kLen) (castPtr vPtr) (fromIntegral vLen)
      case result of
        0 -> return $ Right 0
        otherwise -> return $ Left "Failed in cdb_make_add"

readCdb' :: CDBHandle -> CUInt -> CUInt -> ErrorT Text IO Text
readCdb' cdb pos len = do
  (readResult, val) <- liftIO $ allocaBytes (fromIntegral len) (\val -> do
                           readResult <- cdb_read cdb val len pos 
                           return (readResult, val)
                         )
  case readResult of
    0 -> do
      let cStringLen = (val, (fromIntegral len))
      text <- liftIO $ peekCStringLen cStringLen
      return text
    otherwise -> throwError "unable to read value"


readCdbValue' :: CDBHandle -> ErrorT Text IO Text
readCdbValue' cdb = do
  (CDB vPos vLen _ _) <- liftIO $ peek cdb
  readCdb' cdb vPos vLen

readCdbKey' :: CDBHandle -> ErrorT Text IO Text
readCdbKey' cdb = do
  (CDB _ _ kPos kLen) <- liftIO $ peek cdb
  readCdb' cdb kPos kLen

readCdb :: Text -> CDBHandle -> IO (Either Text Text)
readCdb key cdb = do
  liftIO $ useAsPtr key $ \kPtr kLen -> do
    result <- cdb_find cdb (castPtr kPtr) (fromIntegral kLen)
    if result > 0
    then runErrorT $ readCdbValue' cdb
    else return $ throwError "Failed to find key "

readAll :: CDBFindHandle -> CDBHandle -> ErrorT Text IO [Text]
readAll cdbf cdb = readAll' cdbf []
  where
    readAll' cdbf result = do
      findResult <- liftIO $ cdb_findnext cdbf
      if findResult > 0
      then do
        val <- readCdbValue' cdb
        readAll' cdbf (result ++ [val])
      else return result

readAllCdb :: Text -> CDBHandle -> IO (Either Text [Text])
readAllCdb key cdb = do
  useAsPtr key $ \kPtr kLen -> do
    alloca $ \cdbf -> do
      findInitResult <- cdb_findinit cdbf cdb (castPtr kPtr) (fromIntegral kLen)
      case findInitResult of
        1 -> runErrorT $ readAll cdbf cdb
        otherwise -> return $ Left "Failed in cdb_find_init"

cdbMakeFinish :: WriteCdb IO Int
cdbMakeFinish = WriteCdb $ \cdbm -> do
  result <- cdb_make_finish cdbm
  case result of
    0 -> return $ Right 0
    _ -> return $ Left "Failed in cdb_make_finish"

makeCdb :: FilePath -> WriteCdb IO a -> IO (Either Text Int)
makeCdb fileName action = do
  let tmpFileName = fileName ++ ".tmp"
  fd <- fdFromFile tmpFileName
  cdbResult <- alloca $ \cdbm -> do
                 cdb_make_start cdbm (fromIntegral fd)
                 result <- runWriteCdb (action >> cdbMakeFinish) cdbm
                 return result
  handle <- fdToHandle fd
  hClose handle
  rename tmpFileName fileName
  return cdbResult

useCdb :: FilePath -> ( CDBHandle -> IO a ) -> IO a
useCdb fileName action = do
  fd <- fdFromFile fileName
  cdbResult <- alloca $ \cdb -> do
                 cdb_init cdb (fromIntegral fd)
                 result <- action cdb
                 cdb_free cdb
                 return result
  handle <- fdToHandle fd
  hClose handle
  return cdbResult

readCdbKeyValue' cdb = do
  key <- runErrorT $ readCdbKey' cdb
  value <- runErrorT $ readCdbValue' cdb
  let key' = either (\_ -> "error") (\c -> c) key
      value' = either (\_ -> "error") (\c -> c) value
  TIO.putStrLn $ key' `T.append` ": " `T.append` value'

dumpCdb' :: CDBHandle -> Ptr CUInt -> IO ()
dumpCdb' cdb pos = do
  seqResult <- cdb_seqnext pos cdb
  if seqResult > 0
  then do
    readCdbKeyValue' cdb
    dumpCdb' cdb pos
  else return ()

dumpCdb cdb = do
  alloca $ \posPtr -> do
    poke posPtr (CUInt 2048)
    dumpCdb' cdb posPtr

