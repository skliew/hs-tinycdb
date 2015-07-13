{-# LANGUAGE ForeignFunctionInterface #-}

module TinyCDB where

import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

#include <cdb.h>

data CDB = CDB { position::CInt, length::CInt }
data CDBM = CDBM
type CDBHandle = Ptr CDB

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
    length <- (#peek struct cdb, cdb_vlen) cdbHandle
    return $ CDB position length

instance Storable CDBM where
  alignment _ = #{alignment struct cdb_make}
  sizeOf _ = #{size struct cdb_make}
  peek _ = return CDBM

foreign import ccall unsafe "cdb.h cdb_make_start" cdb_make_start :: Ptr CDBM -> CInt -> IO CInt
foreign import ccall unsafe "cdb.h cdb_make_add" cdb_make_add :: Ptr CDBM -> Ptr Word16 -> CUInt -> Ptr Word16 -> CUInt -> IO CInt
foreign import ccall unsafe "cdb.h cdb_make_exists" cdb_make_exists :: Ptr CDBM -> CString -> CUInt -> IO CInt
foreign import ccall unsafe "cdb.h cdb_make_find" cdb_make_find :: Ptr CDBM -> CString -> CUInt -> CDBPutMode -> IO CInt
foreign import ccall unsafe "cdb.h cdb_make_put" cdb_make_put :: Ptr CDBM -> CString -> CUInt -> CString -> CUInt -> CDBPutMode -> IO CInt
foreign import ccall unsafe "cdb.h cdb_make_finish" cdb_make_finish :: Ptr CDBM -> IO CInt

