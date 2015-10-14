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
  CDBM, CDB(CDB), CDBPutMode, cdb_make_start, cdb_make_add, cdb_make_exists, cdb_make_find, cdb_make_put, cdb_make_finish,
  cdb_init, cdb_find, cdb_free, cdb_read )

fdFromFile :: String -> IO System.Posix.Types.Fd
fdFromFile fileName = do
  handle <- openBinaryFile fileName ReadWriteMode
  fd <- handleToFd handle
  return fd

-- TODO: Provide a Monad to capture failures
addKeyValue cdbm k v = do
  useAsPtr k $ \kPtr kLen -> do
    useAsPtr v $ \vPtr vLen -> do
      cdb_make_add cdbm (castPtr kPtr) (fromIntegral kLen) (castPtr vPtr) (fromIntegral vLen)

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

makeCdb fileName action = do
  let tmpFileName = fileName ++ ".tmp"
  fd <- fdFromFile tmpFileName
  cdbResult <- alloca $ \cdbm -> do
                 cdb_make_start cdbm (fromIntegral fd)
                 action cdbm
                 cdb_make_finish cdbm
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

