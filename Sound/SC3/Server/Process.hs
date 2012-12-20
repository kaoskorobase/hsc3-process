{-# LANGUAGE ExistentialQuantification #-}
-- | This module includes utilities for spawning an external scsynth process,
-- either for realtime or non-realtime execution, and for connecting to existing
-- processes.
module Sound.SC3.Server.Process (
  module Sound.SC3.Server.Process.Options
, OutputHandler(..)
, defaultOutputHandler
, NetworkTransport
, withTransport
, withSynth
, runNRT
, withNRT
) where

import           Control.Applicative ((<$>))
import           Control.Concurrent (forkIO, rtsSupportsBoundThreads)
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import           Control.Exception (Exception(toException), SomeException, bracket, catchJust, throwIO)
import           Control.Monad (liftM, unless, void)
import qualified Data.ByteString.Lazy as B
import           Data.Default (Default(..))
import           Data.List (isPrefixOf)
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
openTransport :: String -> NetworkPort -> IO NetworkTransport
openTransport host (UDPPort p) = NetworkTransport <$> OSC.openUDP host (checkPort "UDP" p)
openTransport host (TCPPort p) = NetworkTransport <$> OSC.openTCP host (checkPort "TCP" p)

-- ====================================================================
-- * Output handler

-- | Handle output of external @scsynth@ processes.
data OutputHandler = OutputHandler {
    onPutString :: String -> IO ()     -- ^ Handle one line of normal output
  , onPutError  :: String -> IO ()     -- ^ Handle one line of error output
  }

instance Default OutputHandler where
  def = defaultOutputHandler

-- | Default IO handler, writing to stdout and stderr, respectively.
defaultOutputHandler :: OutputHandler
defaultOutputHandler = OutputHandler {
    onPutString = \s -> hPutStrLn stdout s >> hFlush stdout
  , onPutError  = \s -> hPutStrLn stderr s >> hFlush stderr }

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

ensureThreadedRuntime :: String -> IO ()
ensureThreadedRuntime fun = unless rtsSupportsBoundThreads $
  error $ "In order to call '"
        ++ fun
        ++ "' without blocking all the other threads in the system,"
        ++ " you must compile the program with -threaded."

-- ====================================================================
-- * Realtime scsynth execution

-- | Open a transport to a running @scsynth@ process determined by 'networkPort'.
withTransport ::
    ServerOptions               -- ^ General server options
 -> RTOptions                   -- ^ Realtime server options
 -> Maybe String                -- ^ Host to connect to (defaults to localhost)
 -> (NetworkTransport -> IO a)  -- ^ Action to execute with the transport
 -> IO a                        -- ^ Action result
withTransport _ rtOptions host =
  bracket
    (openTransport (maybe localhost id host) (networkPort rtOptions))
    OSC.close

-- | Execute a realtime instance of @scsynth@ with 'Transport' t.
--
-- The spawned @scsynth@ is sent a @\/quit@ message after the supplied action
-- returns.
--
-- GHC Note: in order to call @withSynth@ without blocking all the other threads
-- in the system, you must compile the program with @-threaded@.
withSynth ::
    ServerOptions               -- ^ General server options
 -> RTOptions                   -- ^ Realtime server options
 -> OutputHandler               -- ^ Output handler
 -> (NetworkTransport -> IO a)  -- ^ Action to execute with the transport
 -> IO a                        -- ^ Action result
withSynth serverOptions rtOptions handler action = do
  ensureThreadedRuntime "withSynth"
  (_, hOut, hErr, hProc) <- runInteractiveProcess exe args Nothing Nothing
  forkPipe onPutError hErr
  processResult <- newEmptyMVar
  void $ forkIO $ waitForProcess hProc >>= putMVar processResult
  -- Prioritize process exit code over EOF exception.
  result <- catchEOF (liftM Right (loop hOut)) (return . Left)
  exitCode <- takeMVar processResult
  case exitCode of
    ExitSuccess ->
      case result of
        Left ex -> throwIO ex
        Right a -> return a
    ExitFailure _ -> throwIO exitCode
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
      bracket (openTransport localhost (networkPort rtOptions))
              (\t -> OSC.sendOSC t quit >> OSC.close t)
              action
    forkPipe f = void . forkIO . pipeOutput (f handler)

-- ====================================================================
-- * Non-Realtime scsynth execution

-- | Render a NRT score by executing an instance of @scsynth@.
--
-- Since 0.8.0
runNRT ::
    ServerOptions       -- ^ General server options
 -> NRTOptions          -- ^ Non-realtime server options
 -> OutputHandler       -- ^ Output handler
 -> FilePath            -- ^ NRT score file path
 -> IO ()
runNRT serverOptions nrtOptions handler commandFilePath =
   withNRT serverOptions nrtOptions handler $
    \h -> B.readFile commandFilePath >>= B.hPut h

-- | Execute a non-realtime instance of @scsynth@ and pass the process' input
--   handle to /Action/ and return the result.
--
-- GHC Note: in order to call @withNRT@ without blocking all the other threads
-- in the system, you must compile the program with @-threaded@.
--
-- Since 0.8.0
withNRT ::
    ServerOptions       -- ^ General server options
 -> NRTOptions          -- ^ Non-realtime server options
 -> OutputHandler       -- ^ Output handler
 -> (Handle -> IO a)    -- ^ Action
 -> IO a                -- ^ Action result
withNRT serverOptions nrtOptions handler action = do
  ensureThreadedRuntime "withNRT"
  (hIn, hOut, hErr, pid) <- runInteractiveProcess exe args Nothing Nothing
  forkPipe onPutString hOut
  forkPipe onPutError hErr
  processResult <- newEmptyMVar
  void $ forkIO $ waitForProcess pid >>= putMVar processResult
  -- Prioritize process exit code over EOF exception.
  result <- catchEOF (liftM Right (action hIn)) (return . Left)
  exitCode <- takeMVar processResult
  case exitCode of
    ExitSuccess ->
      case result of
        Left ex -> throwIO ex
        Right a -> return a
    ExitFailure _ -> throwIO exitCode
  where
    (exe:args) = nrtCommandLine serverOptions nrtOptions Nothing
    forkPipe f = void . forkIO . pipeOutput (f handler)
