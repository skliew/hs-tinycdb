{-# LANGUAGE OverloadedStrings #-}
module HsTinyCDB where

import System.Posix.IO ( handleToFd, fdToHandle )
import System.Posix.Types ( Fd )
import System.Posix.Files ( rename )
import System.IO
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr ( castPtr )
import Control.Applicative
import Control.Monad ( ap, liftM )
import Control.Monad.Error
import Data.Either ( either )
import Foreign.C.Types ( CUInt(CUInt) )
import Foreign.Ptr ( Ptr )
import Foreign.C.String ( CStringLen )
import Data.ByteString.Unsafe ( unsafeUseAsCStringLen, unsafePackCStringLen )
import Data.ByteString as BS
import Data.ByteString.Char8 as BS8

import TinyCDB

newtype WriteCdb m a = WriteCdb { runWriteCdb :: CDBMHandle -> m (Either ByteString a) }

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

instance Error ByteString where
  noMsg = ""
  strMsg str = BS8.pack str

fdFromFile :: FilePath -> IOMode -> IO System.Posix.Types.Fd
fdFromFile fileName mode = do
  handle <- openBinaryFile fileName mode
  fd <- handleToFd handle
  return fd

addKeyValue :: ByteString -> ByteString -> WriteCdb IO Int
addKeyValue k v = WriteCdb $ \cdbm -> do
  unsafeUseAsCStringLen k $ \(kPtr, kLen) -> do
    unsafeUseAsCStringLen v $ \(vPtr, vLen) -> do
      result <- cdb_make_add cdbm (castPtr kPtr) (fromIntegral kLen) (castPtr vPtr) (fromIntegral vLen)
      case result of
        0 -> return $ Right 0
        otherwise -> return $ Left "Failed in cdb_make_add"

readCdb' :: CDBHandle -> CUInt -> CUInt -> ErrorT ByteString IO ByteString
readCdb' cdb pos len = do
  (readResult, val) <- liftIO $ allocaBytes (fromIntegral len) (\val -> do
                           readResult <- cdb_read cdb val len pos 
                           return (readResult, val)
                         )
  case readResult of
    0 -> do
      let cStringLen = (val, (fromIntegral len))
      text <- liftIO $ unsafePackCStringLen cStringLen
      return text
    otherwise -> throwError "unable to read value"

readCdbValue' :: CDBHandle -> ErrorT ByteString IO ByteString
readCdbValue' cdb = do
  (CDB vPos vLen _ _) <- liftIO $ peek cdb
  readCdb' cdb vPos vLen

readCdb :: ByteString -> CDBHandle -> IO (Either ByteString ByteString)
readCdb key cdb = do
  liftIO $ unsafeUseAsCStringLen key $ \(kPtr, kLen) -> do
    result <- cdb_find cdb (castPtr kPtr) (fromIntegral kLen)
    if result > 0
    then runErrorT $ readCdbValue' cdb
    else return $ throwError "Failed to find key "

readAll :: CDBFindHandle -> CDBHandle -> ErrorT ByteString IO [ByteString]
readAll cdbf cdb = readAll' cdbf []
  where
    readAll' cdbf result = do
      findResult <- liftIO $ cdb_findnext cdbf
      if findResult > 0
      then do
        val <- readCdbValue' cdb
        readAll' cdbf (result ++ [val])
      else return result

readAllCdb :: ByteString -> CDBHandle -> IO (Either ByteString [ByteString])
readAllCdb key cdb = do
  unsafeUseAsCStringLen key $ \(kPtr, kLen) -> do
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

makeCdb :: FilePath -> WriteCdb IO a -> IO (Either ByteString Int)
makeCdb fileName action = do
  let tmpFileName = fileName ++ ".tmp"
  fd <- fdFromFile tmpFileName WriteMode
  cdbResult <- alloca $ \cdbm -> do
                 cdb_make_start cdbm (fromIntegral fd)
                 result <- runWriteCdb (action >> cdbMakeFinish) cdbm
                 return result
  handle <- fdToHandle fd
  hClose handle
  rename tmpFileName fileName
  return cdbResult

-- TODO handle errors
useCdb :: FilePath -> ( CDBHandle -> IO a ) -> IO a
useCdb fileName action = do
  fd <- fdFromFile fileName ReadMode
  cdbResult <- alloca $ \cdb -> do
                 cdb_init cdb (fromIntegral fd)
                 result <- action cdb
                 cdb_free cdb
                 return result
  handle <- fdToHandle fd
  hClose handle
  return cdbResult

readCdbKeyValue' :: CDBHandle -> IO ()
readCdbKeyValue' cdb = do
  (CDB _ _ kPos kLen) <- peek cdb
  key <- runErrorT $ readCdb' cdb kPos kLen
  (CDB vPos vLen _ _ ) <- peek cdb
  key <- runErrorT $ readCdb' cdb kPos kLen
  value <- runErrorT $ readCdb' cdb vPos vLen
  let key' = either (\_ -> "error") (\c -> c) key
      value' = either (\_ -> "error") (\c -> c) value
  BS8.putStrLn $ "+" `BS.append` (BS8.pack $ show kLen) `BS.append` ","
                 `BS.append` (BS8.pack $ show vLen) `BS.append` ":"
                 `BS.append` key' `BS.append` "->" `BS.append` value'


dumpCdb' :: CDBHandle -> Ptr CUInt -> IO ()
dumpCdb' cdb pos = do
  seqResult <- cdb_seqnext pos cdb
  if seqResult > 0
  then do
    readCdbKeyValue' cdb
    dumpCdb' cdb pos
  else do
    BS8.putStrLn ""
    return ()

dumpCdb :: CDBHandle -> IO ()
dumpCdb cdb = do
  alloca $ \posPtr -> do
    poke posPtr (CUInt 2048)
    dumpCdb' cdb posPtr

