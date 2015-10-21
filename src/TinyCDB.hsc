{-# LANGUAGE ForeignFunctionInterface #-}

module TinyCDB where

import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

#include <cdb.h>

data CDB = CDB { vPos::CUInt, vLen::CUInt, kLen::CUInt, kPos::CUInt }
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
    vPos <- (#peek struct cdb, cdb_vpos) cdbHandle
    vLen <- (#peek struct cdb, cdb_vlen) cdbHandle
    kPos <- (#peek struct cdb, cdb_kpos) cdbHandle
    kLen <- (#peek struct cdb, cdb_klen) cdbHandle
    return $ CDB vPos vLen kPos kLen
  poke cdbHandle (CDB vPos vLen kPos kLen) = do
    (#poke struct cdb, cdb_vpos) cdbHandle vPos
    (#poke struct cdb, cdb_vlen) cdbHandle vLen
    (#poke struct cdb, cdb_kpos) cdbHandle kPos
    (#poke struct cdb, cdb_klen) cdbHandle kLen

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

foreign import ccall unsafe "cdb.h cdb_make_start" cdb_make_start :: CDBMHandle -> CInt -> IO CInt
foreign import ccall unsafe "cdb.h cdb_make_add" cdb_make_add :: CDBMHandle -> CString -> CUInt -> CString -> CUInt -> IO CInt
foreign import ccall unsafe "cdb.h cdb_make_exists" cdb_make_exists :: CDBMHandle -> CString -> CUInt -> IO CInt
foreign import ccall unsafe "cdb.h cdb_make_find" cdb_make_find :: CDBMHandle -> CString -> CUInt -> CDBPutMode -> IO CInt
foreign import ccall unsafe "cdb.h cdb_make_put" cdb_make_put :: CDBMHandle -> CString -> CUInt -> CString -> CUInt -> CDBPutMode -> IO CInt
foreign import ccall unsafe "cdb.h cdb_make_finish" cdb_make_finish :: CDBMHandle -> IO CInt

foreign import ccall unsafe "cdb.h cdb_init" cdb_init :: CDBHandle -> CInt -> IO CInt
foreign import ccall unsafe "cdb.h cdb_find" cdb_find :: CDBHandle -> CString -> CUInt -> IO CInt
foreign import ccall unsafe "cdb.h cdb_findinit" cdb_findinit :: CDBFindHandle -> CDBHandle -> CString -> CUInt -> IO CInt
foreign import ccall unsafe "cdb.h cdb_findnext" cdb_findnext :: CDBFindHandle -> IO CInt
foreign import ccall unsafe "cdb.h cdb_read" cdb_read :: CDBHandle -> CString -> CUInt -> CUInt -> IO CInt
foreign import ccall unsafe "cdb.h cdb_free" cdb_free :: CDBHandle -> IO ()

foreign import ccall unsafe "cdb.h cdb_seqnext" cdb_seqnext :: Ptr CUInt -> CDBHandle -> IO CInt

