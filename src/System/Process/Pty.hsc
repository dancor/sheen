module System.Process.Pty where

import Foreign
import Foreign.C
import System.IO
import System.Posix.Types
import System.Posix.IO
import System.Process.Internals hiding (fdToHandle)

#include "sheen.h"

foreign import ccall "static sheen.h run_pty_process"
    c_run_pty_process :: Ptr CString -> Ptr CInt -> IO PHANDLE

runPtyProcess :: [String] -> IO (Handle, ProcessHandle)
runPtyProcess args =
  alloca $ \ fdMasterPtr ->
  withMany withCString args $ \ cstrs ->
  withArray0 nullPtr cstrs $ \ pargs -> do
    procHandleInt <- throwErrnoIfMinus1 "runPtyProcess" $
      c_run_pty_process pargs fdMasterPtr
    fdMaster <- peek fdMasterPtr
    putStrLn $ "pid:" ++ show procHandleInt
    hMaster <- fdToHandle (Fd fdMaster)
    procHandle <- mkProcessHandle procHandleInt
    hSetBuffering hMaster NoBuffering
    return (hMaster, procHandle)
