module HsTinyCDB where

import System.Posix.IO
import System.Posix.Types
import System.IO
import Data.Text
import Foreign.Marshal.Alloc
import Data.Map
import Data.Text.Foreign

import TinyCDB ( CDBM, CDB, CDBPutMode, cdb_make_start, cdb_make_add, cdb_make_exists, cdb_make_find, cdb_make_put, cdb_make_finish )

fdFromFile :: String -> IO System.Posix.Types.Fd
fdFromFile fileName = do
  handle <- openBinaryFile fileName ReadWriteMode
  fd <- handleToFd handle
  return fd

addKeyValue cdbm (k, v) = do
  useAsPtr k $ \kPtr kLen -> do
    useAsPtr v $ \vPtr vLen -> do
      cdb_make_add cdbm kPtr (fromIntegral kLen) vPtr (fromIntegral vLen)

makeCdb fp dict = do
  fd <- fdFromFile $ fp ++ ".tmp"
  alloca $ \cdbm -> do
    cdb_make_start cdbm (fromIntegral fd)
    let keyValuePairs = assocs dict
    mapM_ (addKeyValue cdbm) keyValuePairs
    cdb_make_finish cdbm
  handle <- fdToHandle fd
  hClose handle


