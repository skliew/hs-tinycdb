{-# LANGUAGE OverloadedStrings #-}
module HsTinyCDB where

import System.Posix.IO
import System.Posix.Types
import System.Posix.Files
import System.IO
import Data.Text
import Foreign.Marshal.Alloc
import Data.Map
import Data.Text.Foreign
import Foreign.Storable
import Foreign.Ptr ( castPtr )
import Control.Applicative
import Control.Monad ( ap, liftM )
import Data.Maybe ( maybeToList )

import TinyCDB (
  CDBFindHandle, CDBMHandle, CDB(CDB), CDBPutMode, cdb_make_start, cdb_make_add, cdb_make_exists, cdb_make_find, cdb_make_put, cdb_make_finish,
  cdb_init, cdb_find, cdb_free, cdb_read, cdb_findinit, cdb_findnext
  )

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

readCdb' cdb = do
  (CDB vPos vLen) <- peek cdb
  val <- allocaBytes (fromIntegral vLen) (\val -> do
           readResult <- cdb_read cdb val vLen vPos
           case readResult of
             0 -> do
               let cStringLen = (val, (fromIntegral vLen))
               text <- peekCStringLen cStringLen
               fmap Just (peekCStringLen cStringLen)
             otherwise -> return $ Nothing
         )
  return val

readCdb key cdb = do
  useAsPtr key $ \kPtr kLen -> do
    result <- cdb_find cdb (castPtr kPtr) (fromIntegral kLen)
    if result > 0
    then do
      val <- readCdb' cdb
      return $ Right val
    else return $ Left $ "Failed to find key "

readAll cdbf cdb = readAll' cdbf []
  where
    readAll' cdbf result = do
      findResult <- cdb_findnext cdbf
      if findResult > 0
      then do
        val <- readCdb' cdb
        let result' = result ++ (maybeToList val)
        readAll' cdbf result'
      else return result

readAllCdb key cdb = do
  useAsPtr key $ \kPtr kLen -> do
    alloca $ \cdbf -> do
      findInitResult <- cdb_findinit cdbf cdb (castPtr kPtr) (fromIntegral kLen)
      case findInitResult of
        1 -> do
          values <- readAll cdbf cdb
          return $ Right values
        otherwise -> return $ Left "Failed in cdb_find_init"

cdbMakeFinish = WriteCdb $ \cdbm -> do
  result <- cdb_make_finish cdbm
  case result of
    0 -> return $ Right 0
    _ -> return $ Left "Failed in cdb_make_finish"

makeCdb :: FilePath -> WriteCdb IO Int -> IO (Either Text Int)
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

