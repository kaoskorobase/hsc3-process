{-# LANGUAGE ScopedTypeVariables #-}
-- | This module includes utilities for spawning an external scsynth process,
-- either for realtime or non-realtime execution.
module Sound.SC3.Server.Process (
    module Sound.SC3.Server.Options,
    OpenTransport(..),
    OutputHandler(..),
    defaultOutputHandler,
    withTransport,
    withSynth,
    withNRT
) where

import Sound.OpenSoundControl               (Transport, TCP, UDP, openTCP, openUDP, send)
import Control.Concurrent
import Control.Exception
import Control.Monad                        (unless)
import Prelude hiding                       (catch)
import Data.List                            (isPrefixOf)

import Sound.SC3                            (quit)
import Sound.SC3.Server.Options
import Sound.SC3.Server.Process.CommandLine
import Sound.SC3.Server.Transport

import System.Exit                          (ExitCode(..))
import System.IO                            (Handle, hGetLine, hIsEOF, hPutStrLn, stderr, stdout)
import System.Process                       (runInteractiveProcess, waitForProcess)

hostName :: Maybe String -> String
hostName = maybe "127.0.0.1" id

-- | Check wether a network port is within the valid range (0, 65535]
checkPort :: String -> Int -> Int
checkPort tag p | p <= 0 || p > 65535 = error ("Invalid " ++ tag ++ " port " ++ show p)
checkPort _ p                         = p

instance OpenTransport (UDP) where
    openTransport _ options server = openUDP (hostName server) (checkPort "UDP" $ udpPortNumber options)

instance OpenTransport (TCP) where
    openTransport _ options server = openTCP (hostName server) (checkPort "TCP" $ tcpPortNumber options)

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

-- ====================================================================
-- * Realtime scsynth execution

withTransport :: (Transport t, OpenTransport t) =>
    ServerOptions
 -> RTOptions
 -> (t -> IO a)
 -> IO a
withTransport serverOptions rtOptions action = do
    fd <- openTransport serverOptions rtOptions Nothing
    action fd

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
        result <- newEmptyMVar
        thread <- forkIO (loop hOut result)
        exitCode <- waitForProcess hProc
        case exitCode of
            ExitSuccess -> do
                a <- readMVar result
                case a of
                    Left e  -> throw e
                    Right a -> return a
            ExitFailure _ -> do
                killThread thread
                throw (toException exitCode)
    where
        (exe:args) = rtCommandLine serverOptions rtOptions
        loop h result = do
            l <- try (hGetLine h)
            case l of
                Left (ex :: IOException) -> returnExc (toException ex)
                Right l ->
                    if "SuperCollider 3 server ready." `isPrefixOf` l
                        then do
                            e <- try (onPutString handler l)
                            case e of
                                Left (ex :: IOException) -> returnExc (toException ex)
                                _                        -> do
                                    forkIO $ putStdout h
                                    fd <- openTransport serverOptions rtOptions Nothing
                                    a <- try (action fd >>= evaluate)
                                    send fd quit
                                    case a of
                                        Left (ex :: SomeException) -> returnExc (toException ex)
                                        Right a -> returnRes a
                        else do
                            e <- try (onPutString handler l)
                            case e of
                                Left (ex :: IOException) -> returnExc (toException ex)
                                _                        -> loop h result -- recurse
            where
                returnRes a = putMVar result (Right a)
                returnExc e = putMVar result (Left e)
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
        result <- newEmptyMVar
        thread <- forkIO $ do
            a <- try (action hIn)
            case a of
                Left (ex :: SomeException) -> putMVar result (Left ex)
                _                          -> putMVar result a
        exitCode <- waitForProcess hProc
        case exitCode of
            ExitSuccess -> do
                a <- readMVar result
                case a of
                    Left e  -> throw e
                    Right a -> return a
            ExitFailure _ -> do
                killThread thread
                throw (toException exitCode)
    where
        (exe:args) = nrtCommandLine serverOptions nrtOptions { commandFilePath = Nothing }
        putStdout = pipeOutput (onPutString handler)
        putStderr = pipeOutput (onPutString handler)

-- EOF
