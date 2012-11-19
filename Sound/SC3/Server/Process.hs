{-# LANGUAGE ExistentialQuantification
           , ScopedTypeVariables #-}
-- | This module includes utilities for spawning an external scsynth process,
-- either for realtime or non-realtime execution.
module Sound.SC3.Server.Process
  ( module Sound.SC3.Server.Process.Options
  , OutputHandler(..)
  , defaultOutputHandler
  , NetworkTransport
  , withTransport
  , withSynth
  , withNRT ) where

import           Control.Applicative ((<$>))
import           Control.Concurrent (forkIO, killThread)
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import           Control.Exception (Exception(toException), SomeException, bracket, catchJust, finally, throw, try)
import           Data.List (isPrefixOf)
import           Control.Monad (liftM)
import           Sound.OSC.FD (Transport(..))
import qualified Sound.OSC.FD as OSC
import           Sound.SC3 (quit)
import           Sound.SC3.Server.Process.CommandLine
import           Sound.SC3.Server.Process.Options
import           System.Exit (ExitCode(..))
import           System.IO (Handle, hFlush, hGetLine, hPutStrLn, stderr, stdout)
import           System.IO.Error (isEOFError)
import           System.Process (runInteractiveProcess, waitForProcess)

localhost :: String
localhost = "127.0.0.1"

-- | Check wether a network port is within the valid range (0, 65535]
checkPort :: String -> Int -> Int
checkPort tag p | p <= 0 || p > 65535 = error ("Invalid " ++ tag ++ " port " ++ show p)
checkPort _ p                         = p

-- | Network transport wrapper.
data NetworkTransport = forall t . Transport t => NetworkTransport t

instance Transport NetworkTransport where
    recvPacket (NetworkTransport t) = recvPacket t
    sendOSC (NetworkTransport t) = sendOSC t
    close (NetworkTransport t) = close t

-- | Open a network transport connected to a network port.
openTransport :: NetworkPort -> IO NetworkTransport
openTransport (UDPPort p) = NetworkTransport <$> OSC.openUDP localhost (checkPort "UDP" p)
openTransport (TCPPort p) = NetworkTransport <$> OSC.openTCP localhost (checkPort "TCP" p)

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
    onPutString = \s -> hPutStrLn stdout s >> hFlush stdout
  , onPutError  = \s -> hPutStrLn stderr s >> hFlush stderr
  }

-- ====================================================================
-- Process helpers

-- | Catch EOF errors.
catchEOF :: IO a -> (SomeException -> IO a) -> IO a
catchEOF = catchJust (\e -> if isEOFError e then Just (toException e) else Nothing)

-- | Continuously pipe lines from a handle to an output function.
--
-- Stop when encountering EOF.
pipeOutput :: (String -> IO ()) -> Handle -> IO ()
pipeOutput f h =
    catchEOF (hGetLine h >>= f >> pipeOutput f h)
             (\_ -> return ())

-- ====================================================================
-- * Realtime scsynth execution

withTransport ::
    ServerOptions
 -> RTOptions
 -> (NetworkTransport -> IO a)
 -> IO a
withTransport _ rtOptions = bracket (openTransport (networkPort rtOptions)) OSC.close

-- | Execute a realtime instance of @scsynth@ with 'Transport' t.
--
-- The spawned @scsynth@ is sent a @\/quit@ message after the supplied action
-- returns.
--
-- /NOTE/: When compiling executables with GHC, the @-threaded@ option should
-- be passed, otherwise the I\/O handlers will not work correctly.
withSynth ::
    ServerOptions
 -> RTOptions
 -> OutputHandler
 -> (NetworkTransport -> IO a)
 -> IO a
withSynth serverOptions rtOptions handler action = do
    (_, hOut, hErr, hProc) <- runInteractiveProcess exe args Nothing Nothing
    forkPipe onPutError hErr
    exitCode <- newEmptyMVar
    forkIO $ waitForProcess hProc >>= putMVar exitCode
    a <- catchEOF (liftM Right (loop hOut)) (return . Left)
    e <- takeMVar exitCode
    case e of
        ExitSuccess ->
            case a of
                Left e -> throw e
                Right a' -> return a'
        ExitFailure _ -> throw e
    where
        (exe:args) = rtCommandLine serverOptions rtOptions
        loop h = do
            l <- hGetLine h
            onPutString handler l
            if "SuperCollider 3 server ready" `isPrefixOf` l
                then cont h
                else loop h
        cont h = do
            forkPipe onPutString h
            fd <- openTransport (networkPort rtOptions)
            action fd `finally` OSC.sendOSC fd quit
        forkPipe f = forkIO . pipeOutput (f handler)

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
        (exe:args) = nrtCommandLine serverOptions nrtOptions
        putStdout = pipeOutput (onPutString handler)
        putStderr = pipeOutput (onPutError handler)
