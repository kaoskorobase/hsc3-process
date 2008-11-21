-- | This module includes utilities for spawning an external scsynth process,
-- either for realtime or non-realtime execution.
module Sound.SC3.Server.Process (
    module Sound.SC3.Server.Process.Options,
    commandLine,
    EventHandler(..),
    _onBoot,
    _onPutString,
    _onPutError,
    defaultEventHandler,
    withSynth,
    withNRT
) where

import Sound.OpenSoundControl               (Transport, TCP, UDP, openTCP, openUDP)
import Control.Concurrent                   (forkIO)
import Control.Monad                        (unless)
import Prelude hiding                       (catch)
import Data.List                            (isPrefixOf)

import Sound.SC3.Server.Process.Accessor    (deriveAccessors)
import Sound.SC3.Server.Process.Options
import Sound.SC3.Server.Process.CommandLine

import System.Exit                          (ExitCode)
import System.IO                            (Handle, hGetLine, hIsEOF, hPutStrLn, stderr, stdout)
import System.Process                       (runInteractiveProcess, waitForProcess)

-- ====================================================================
-- * Realtime options

-- | Helper class for polymorphic opening of network connections.
class OpenTransport t where
    -- type TEventHandler t
    openTransport :: RTOptions -> String -> IO t

checkPort :: String -> Int -> Int
checkPort tag p | p <= 0 || p > 65535 = error ("Invalid " ++ tag ++ " port " ++ show p)
checkPort _ p                         = p

instance OpenTransport (UDP) where
    -- type TEventHandler UDP = EventHandler UDP
    openTransport options server = openUDP server (checkPort "UDP" $ options^.udpPortNumber)

instance OpenTransport (TCP) where
    -- type TEventHandler TCP = EventHandler TCP
    openTransport options server = openTCP server (checkPort "TCP" $ options^.tcpPortNumber)

-- ====================================================================
-- * Event handler

-- | Event handler for handling I/O with external @scsynth@ processes,
-- parameterized by the I/O handle used for sending OSC commands to the
-- server.
data EventHandler t = EventHandler {
    onPutString :: String -> IO (),     -- ^ Handle one line of normal output
    onPutError  :: String -> IO (),     -- ^ Handle one line of error output
    onBoot      :: t -> IO ()           -- ^ Executed with the OSC handle after the server has booted
}

$(deriveAccessors ''EventHandler)

-- | Default event handler, writing to stdout and stderr, respectively.
defaultEventHandler :: EventHandler t
defaultEventHandler = EventHandler {
    onPutString = hPutStrLn stdout,
    onPutError  = hPutStrLn stderr,
    onBoot      = const (return ())
}

-- ====================================================================
-- Process helpers

pipeOutput :: (String -> IO ()) -> Handle -> IO ()
pipeOutput f h = hIsEOF h >>= flip unless (hGetLine h >>= f >> pipeOutput f h)

-- ====================================================================
-- * Realtime scsynth execution

-- | Execute a realtime instance of @scsynth@ with 'Transport' t and return
-- 'ExitCode' when the process exists.
--
-- /NOTE/: When compiling executables with GHC, the @-threaded@ option should be
-- passed, otherwise the I\/O handlers will not work correctly.
withSynth :: (Transport t, OpenTransport t) =>
    ServerOptions
 -> RTOptions
 -> EventHandler t
 -> IO ExitCode
withSynth serverOptions rtOptions handler = do
        print (exe, args)
        (_, hOut, hErr, hProc) <- runInteractiveProcess exe args Nothing Nothing
        forkIO $ putStdout0 hOut
        forkIO $ putStderr  hErr
        waitForProcess hProc
    where
        (exe:args) = commandLine serverOptions rtOptions
        putStdout0 h = do
            eof <- hIsEOF h
            unless eof $ do
                l <- hGetLine h
                if isPrefixOf "SuperCollider 3 server ready.." l
                    then do
                        onPutString handler l
                        fd <- openTransport rtOptions "127.0.0.1"
                        forkIO $ onBoot handler fd
                        -- Spawn more efficient output handler
                        forkIO $ putStdout h
                        return ()
                    else do
                        onPutString handler l
                        putStdout0 h -- recurse
        putStdout = pipeOutput (onPutString handler)
        putStderr = pipeOutput (onPutError  handler)
    
-- ====================================================================
-- * Non-Realtime scsynth execution

-- | Execute a non-realtime instance of @scsynth@ and return 'ExitCode' when
-- the process exists.
withNRT ::
    ServerOptions
 -> NRTOptions
 -> EventHandler Handle
 -> IO ExitCode
withNRT serverOptions nrtOptions handler = do
        (hIn, hOut, hErr, hProc) <- runInteractiveProcess exe args Nothing Nothing
        forkIO $ putStdout hOut
        forkIO $ putStderr hErr
        forkIO $ onBoot handler hIn
        waitForProcess hProc
    where
        (exe:args) = commandLine serverOptions nrtOptions { commandFilePath = Nothing }
        putStdout = pipeOutput (onPutString handler)
        putStderr = pipeOutput (onPutString handler)

-- EOF
