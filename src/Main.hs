module Main where

import Control.Concurrent
import Control.Exception hiding (catch)
import Control.Monad
import Data.BQueue
import System.Process.Pty
import System.IO
import System.Posix.Terminal
import System.Posix.Types
import System.Posix.IO
import System.Process
import Util

-- state for a single process
data Window = Window {
  wH       :: Handle,
  wPid     :: ProcessHandle,
  -- partially read escape sequence
  wEscBuff :: String,
  -- the scrBuff is the current window and scroll back, so different structure
  -- could be better..
  -- currently
  wScrBuff :: BQ (BQ Char)}
  -- to add: scroll state?

-- state for a collection of Window's (i.e. an instance of a sheen server)
data Session = Session {
  snShellCmd :: [String],
  -- might become MVar?
  snWins     :: [Window],
  -- might become Maybe for empty win..
  snCurWinIx :: MVar Int,
  -- partially read command
  --    this could really just exist as accumulator in passInp,
  --    but convenient here?
  snKeyBuff  :: Maybe String,
  -- escape key for session
  snEsc      :: Char,
  -- char following escape key that produces literal snEsc
  snEsc2     :: Char}

-- check if a key sequence is a legit command
-- if it is do the command and return Nothing
-- if it is not: if it is a valid command prefix return Just it, else Nothing
tryCmd :: Session -> String -> IO (Session, Maybe String)
tryCmd sn s = let curWinIx = snCurWinIx sn in case s of
  "N" -> do
    n <- readMVar curWinIx
    putStrLn $ "This is window " ++ show n
    return (sn, Nothing)
  "1" -> do
    -- FIXME: display "This IS window 1" if already is?
    swapMVar curWinIx 1
    return (sn, Nothing)
  _ -> return (sn, Nothing)

-- filter input and forward input to the current window
-- FIXME?: we could make the entire hotkey stuff completely general
--    but maybe it is a good idea to hard code snEsc/snEsc2 anyway?
--    for one thing, having all commands start with same snEsc makes it easier
--    to embed screens by having different snEsc's with everything else the
--    same, right?  but that could still be done by choice instead of force..
--    another thing, there might be a perf gain by having most chars (i.e all
--    but snEsc) not initiate hotkey stuff
passInp :: Session -> IO ()
passInp sn = do
  c <- getChar
  let esc = snEsc sn
  -- FIXME?: maybe buff' should be inserted individually?  see how tryCmd plays
  -- out before deciding
  (sn', cMb, buff') <- case snKeyBuff sn of
    Nothing -> return $ if c == esc
      then (sn, Nothing, Just [])
      else (sn, Just c, Nothing)
    Just buff -> if c == snEsc2 sn && null buff
      then return (sn, Just esc, Nothing)
      else do
        (snRet, buffRet) <- tryCmd sn (buff ++ [c])
        return (snRet, Nothing, buffRet)
  case cMb of
    Nothing -> return ()
    Just c' -> do
      curWinIx <- readMVar (snCurWinIx sn)
      let h = wH $ (snWins sn') !! curWinIx
      hPutChar h c'
  passInp sn' {snKeyBuff = buff'}

-- filter output and forward output from the current window
passOut :: Session -> IO ()
passOut sn = do
  curWinIx <- readMVar (snCurWinIx sn)
  let h = wH $ (snWins sn) !! curWinIx
  -- catch errors bc when process finishes this thread will see a read fault
  cMb <- hideErrsMb $ hGetChar h
  case cMb of
    -- FIXME: uh this return () won't work once we have multiple windows right?
    -- or maybe it will never come to this until all windows are gone
    -- might have to take care to play our curWinIx cards right..
    Nothing -> return ()
    Just c  -> {-case c of-} putChar c >> passOut sn
    --Just c  -> print c >> passOut h

hideErrs :: IO () -> IO ()
hideErrs io = catch io (\ _ -> return ())

hideErrsMb :: IO a -> IO (Maybe a)
hideErrsMb io = catch (io >>= return . Just) $ \ _ -> return Nothing

-- changes the term to raw and returns the original term attributes (so you can
-- restore them later)
termGoRaw :: Fd -> IO TerminalAttributes
termGoRaw fd = do
  attrs <- getTerminalAttributes fd
  let
    attrs' = dlof (withBits attrs 8) $ map (flip withoutMode) [
      EnableEcho, ProcessInput, ExtendedFunctions, KeyboardInterrupts,
      InterruptOnBreak, MapCRtoLF, CheckParity, StripHighBit, StartStopOutput,
      EnableParity, ProcessOutput]
  -- go raw
  setTerminalAttributes fd attrs' Immediately
  return attrs

onTermChange :: (Fd -> IO TerminalAttributes) -> Fd -> IO a -> IO a
onTermChange f fd io = do
  termAttrs <- f fd
  finally io $ setTerminalAttributes fd termAttrs Immediately

onTermRaw :: Fd -> IO a -> IO a
onTermRaw = onTermChange termGoRaw

-- wait until a handle gets to eof (presumably from another thread reading to
-- the end) or another abortive exception happens
-- (e.g. a read fault occurs when a pty you are reading from goes away)
hWaitForEnd :: Handle -> IO ()
hWaitForEnd h = hideErrs $ do
    r <- hWaitForInput h $ -1
    -- should we insert a delay before redoing?  could have unnecessary cpu.
    -- but it is a trade off of cpu and exit delay and doesn't seem to be ish.
    when r $ hWaitForEnd h

makeWin :: [String] -> IO Window
makeWin shellCmd = do
  (h,pid) <- runPtyProcess shellCmd
  return $ Window {
    wH       = h,
    wPid     = pid,
    wEscBuff = []}

main :: IO ()
main = do
  let
    scrNum   = 2
    shellCmd = ["/bin/sh", "-i"]
  wins     <- replicateM scrNum $ makeWin shellCmd
  curWinIx <- newMVar 0
  let
    sn = Session {
      snShellCmd = shellCmd,
      --snLogH     = logH,
      --snScrW     =
      snWins     = wins,
      snCurWinIx = curWinIx,
      snKeyBuff  = Nothing,
      snEsc      = '\^A',
      snEsc2     = 'a'}
  -- to check: both these needed?
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  onTermRaw stdInput $ do
    forkIO $ passInp sn
    forkIO $ passOut sn
    waitForProcess . wPid $ head wins
    waitForProcess . wPid $ last wins
    -- maybe it would be more correct to wait on passOut thread
    -- (would need an MVar)
    hWaitForEnd . wH $ head wins
