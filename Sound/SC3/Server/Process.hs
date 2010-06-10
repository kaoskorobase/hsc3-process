-- | This module includes utilities for spawning an external scsynth process,
-- either for realtime or non-realtime execution.
module Sound.SC3.Server.Process (
    module Sound.SC3.Server.Process.Options,
    OpenTransport(..),
    OutputHandler(..),
    defaultOutputHandler,
    withSynth,
    withNRT
) where

import Sound.OpenSoundControl               (Transport, TCP, UDP, openTCP, openUDP, send)
import Control.Concurrent                   (ThreadId, forkIO, myThreadId, throwTo)
import Control.Exception                    (finally)
import Control.Monad                        (unless)
import Prelude hiding                       (catch)
import Data.List                            (isPrefixOf)

import Sound.SC3                            (quit)
import Sound.SC3.Server.Process.Accessor    (deriveAccessors)
import Sound.SC3.Server.Process.Options
import Sound.SC3.Server.Process.CommandLine

import System.Exit                          (ExitCode(..))
import System.IO                            (Handle, hGetLine, hIsEOF, hPutStrLn, stderr, stdout)
import System.Process                       (ProcessHandle, runInteractiveProcess, waitForProcess)

-- | Helper class for polymorphic opening of network connections.
class OpenTransport t where
    -- | Open a transport to scsynth based on the given RTOptions and a hostname.
    openTransport :: RTOptions -> String -> IO t

-- | Check wether a network port is within the valid range (0, 65535]
checkPort :: String -> Int -> Int
checkPort tag p | p <= 0 || p > 65535 = error ("Invalid " ++ tag ++ " port " ++ show p)
checkPort _ p                         = p

instance OpenTransport (UDP) where
    openTransport options server = openUDP server (checkPort "UDP" $ udpPortNumber options)

instance OpenTransport (TCP) where
    openTransport options server = openTCP server (checkPort "TCP" $ tcpPortNumber options)

-- ====================================================================
-- * Output handler

-- | Handle output of external @scsynth@ processes.
data OutputHandler = OutputHandler {
    onPutString :: String -> IO ()     -- ^ Handle one line of normal output
  , onPutError  :: String -> IO ()     -- ^ Handle one line of error output
}

-- | Default IO handler, writing to stdout and stderr, respectively.
defaultOutputHandler :: OutputHandler
defaultOutputHandler = OutputHandler {
    onPutString = hPutStrLn stdout
  , onPutError  = hPutStrLn stderr
}

-- ====================================================================
-- Process helpers

pipeOutput :: (String -> IO ()) -> Handle -> IO ()
pipeOutput f h = hIsEOF h >>= flip unless (hGetLine h >>= f >> pipeOutput f h)

watchProcess :: ProcessHandle -> ThreadId -> IO ()
watchProcess pid tid = do
    e <- waitForProcess pid
    case e of
        ExitSuccess -> return ()
        ex          -> throwTo tid ex

-- ====================================================================
-- * Realtime scsynth execution

-- | Execute a realtime instance of @scsynth@ with 'Transport' t.
--
-- The spawned @scsynth@ is sent a @\/quit@ message after the supplied action
-- returns.
--
-- /NOTE/: When compiling executables with GHC, the @-threaded@ option should
-- be passed, otherwise the I\/O handlers will not work correctly.
withSynth :: (Transport t, OpenTransport t) =>
    ServerOptions
 -> RTOptions
 -> OutputHandler
 -> (t -> IO a)
 -> IO a
withSynth serverOptions rtOptions handler action = do
        (_, hOut, hErr, hProc) <- runInteractiveProcess exe args Nothing Nothing
        forkIO $ putStderr hErr
        myThreadId >>= forkIO . watchProcess hProc
        loop hOut
    where
        (exe:args) = rtCommandLine serverOptions rtOptions
        loop h = do
            l <- hGetLine h
            if "SuperCollider 3 server ready.." `isPrefixOf` l
                then do
                    onPutString handler l
                    -- Spawn output handler
                    forkIO $ putStdout h
                    fd <- openTransport rtOptions "127.0.0.1"
                    action fd `finally` send fd quit
                else do
                    onPutString handler l
                    loop h -- recurse
        putStdout = pipeOutput (onPutString handler)
        putStderr = pipeOutput (onPutError  handler)

-- ====================================================================
-- * Non-Realtime scsynth execution

-- | Execute a non-realtime instance of @scsynth@ and return 'ExitCode' when
-- the process exists.
withNRT ::
    ServerOptions
 -> NRTOptions
 -> OutputHandler
 -> (Handle -> IO a)
 -> IO a
withNRT serverOptions nrtOptions handler action = do
        (hIn, hOut, hErr, hProc) <- runInteractiveProcess exe args Nothing Nothing
        forkIO $ putStdout hOut
        forkIO $ putStderr hErr
        myThreadId >>= forkIO . watchProcess hProc
        action hIn
    where
        (exe:args) = nrtCommandLine serverOptions nrtOptions { commandFilePath = Nothing }
        putStdout = pipeOutput (onPutString handler)
        putStderr = pipeOutput (onPutString handler)

-- EOF
