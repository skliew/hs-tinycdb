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

import TinyCDB (
  CDBMHandle, CDB(CDB), CDBPutMode, cdb_make_start, cdb_make_add, cdb_make_exists, cdb_make_find, cdb_make_put, cdb_make_finish,
  cdb_init, cdb_find, cdb_free, cdb_read )

data WriteCdb m a = WriteCdb { runWriteCdb :: m (Either String a) }

instance Monad m => Monad (WriteCdb m) where
  return a = WriteCdb . return $ Right a
  x >>= f = WriteCdb $ do
              value <- runWriteCdb x
              case value of
                Left error -> return $ Left error
                Right v -> runWriteCdb $ f v

fdFromFile :: String -> IO System.Posix.Types.Fd
fdFromFile fileName = do
  handle <- openBinaryFile fileName ReadWriteMode
  fd <- handleToFd handle
  return fd

addKeyValue :: CDBMHandle -> Text -> Text -> WriteCdb IO Int
addKeyValue cdbm k v = WriteCdb $ do
  useAsPtr k $ \kPtr kLen -> do
    useAsPtr v $ \vPtr vLen -> do
      result <- cdb_make_add cdbm (castPtr kPtr) (fromIntegral kLen) (castPtr vPtr) (fromIntegral vLen)
      case result of
        0 -> return $ Right 0
        otherwise -> return $ Left "Failed in cdb_make_add"

readCdb' cdb key = do
  (CDB vPos vLen) <- peek cdb
  val <- allocaBytes (fromIntegral vLen) (\val -> do
           cdb_read cdb val vLen vPos
           -- Is there a better way to construct a CStringLen?
           let cStringLen = (val, (fromIntegral vLen))
           peekCStringLen cStringLen
         )
  return val

readCdb cdb key = do
  useAsPtr key $ \kPtr kLen -> do
    result <- cdb_find cdb (castPtr kPtr) (fromIntegral kLen)
    if result > 0
    then do
      val <- readCdb' cdb key
      return $ Just val
    else return Nothing

cdbMakeFinish cdbm = WriteCdb $ do
  result <- cdb_make_finish cdbm
  case result of
    0 -> return $ Right 0
    _ -> return $ Left "Failed in cdb_make_finish"

makeCdb :: String -> (CDBMHandle -> WriteCdb IO Int) -> IO (Either String Int)
makeCdb fileName action = do
  let tmpFileName = fileName ++ ".tmp"
  fd <- fdFromFile tmpFileName
  cdbResult <- alloca $ \cdbm -> do
                 cdb_make_start cdbm (fromIntegral fd)
                 result <- runWriteCdb $ do
                             action cdbm
                             cdbMakeFinish cdbm
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
  fmap hClose (fdToHandle fd)
  return cdbResult

