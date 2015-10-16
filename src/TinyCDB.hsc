{-# LANGUAGE ForeignFunctionInterface #-}

module TinyCDB where

import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

#include <cdb.h>

data CDB = CDB { position::CUInt, len::CUInt }
data CDBM = CDBM
data CDBFind = CDBFind
type CDBHandle = Ptr CDB
type CDBMHandle = Ptr CDBM
type CDBFindHandle = Ptr CDBFind

newtype CDBPutMode = CDBPutMode { mode :: CInt }
#{enum CDBPutMode, CDBPutMode,
  putAdd = CDB_PUT_ADD,
  putFind = CDB_PUT_ADD,
  putReplace = CDB_PUT_REPLACE,
  putFindRemove = CDB_PUT_REPLACE,
  putInsert = CDB_PUT_INSERT,
  putWarn = CDB_PUT_WARN,
  putReplace0 = CDB_PUT_REPLACE0,
  findReplace0 = CDB_PUT_REPLACE0
}

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable CDB where
  alignment _ = #{alignment struct cdb}
  sizeOf _ = #{size struct cdb}
  peek cdbHandle = do
    position <- (#peek struct cdb, cdb_vpos) cdbHandle
    len <- (#peek struct cdb, cdb_vlen) cdbHandle
    return $ CDB position len
  poke cdbHandle (CDB position len) = do
    (#poke struct cdb, cdb_vpos) cdbHandle position
    (#poke struct cdb, cdb_vlen) cdbHandle len

instance Storable CDBFind where
  alignment _ = #{alignment struct cdb_find}
  sizeOf _ = #{size struct cdb_find}
  peek cdbFindHandle = return CDBFind
  poke cdbFindHandle cdbFind = return ()

instance Storable CDBM where
  alignment _ = #{alignment struct cdb_make}
  sizeOf _ = #{size struct cdb_make}
  peek _ = return CDBM
  poke cdbmHandle cdbm = return ()

foreign import ccall unsafe "cdb.h cdb_make_start" cdb_make_start :: Ptr CDBM -> CInt -> IO CInt
foreign import ccall unsafe "cdb.h cdb_make_add" cdb_make_add :: Ptr CDBM -> CString -> CUInt -> CString -> CUInt -> IO CInt
foreign import ccall unsafe "cdb.h cdb_make_exists" cdb_make_exists :: Ptr CDBM -> CString -> CUInt -> IO CInt
foreign import ccall unsafe "cdb.h cdb_make_find" cdb_make_find :: Ptr CDBM -> CString -> CUInt -> CDBPutMode -> IO CInt
foreign import ccall unsafe "cdb.h cdb_make_put" cdb_make_put :: Ptr CDBM -> CString -> CUInt -> CString -> CUInt -> CDBPutMode -> IO CInt
foreign import ccall unsafe "cdb.h cdb_make_finish" cdb_make_finish :: Ptr CDBM -> IO CInt

foreign import ccall unsafe "cdb.h cdb_init" cdb_init :: Ptr CDB -> CInt -> IO CInt
foreign import ccall unsafe "cdb.h cdb_find" cdb_find :: Ptr CDB -> CString -> CUInt -> IO CInt
foreign import ccall unsafe "cdb.h cdb_findinit" cdb_findinit :: Ptr CDBFind -> Ptr CDB -> CString -> CUInt -> IO CInt
foreign import ccall unsafe "cdb.h cdb_findnext" cdb_findnext :: Ptr CDBFind -> IO CInt
foreign import ccall unsafe "cdb.h cdb_read" cdb_read :: Ptr CDB -> CString -> CUInt -> CUInt -> IO CInt
foreign import ccall unsafe "cdb.h cdb_free" cdb_free :: Ptr CDB -> IO ()

