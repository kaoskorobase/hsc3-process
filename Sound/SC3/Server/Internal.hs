module Sound.SC3.Server.Internal (
    InternalTransport
  , withInternal
) where

import           Bindings.Sound.SC3
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Exception (bracket)
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.State as State
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import           Foreign.C
import           Foreign.ForeignPtr
import           Foreign.Marshal
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.StablePtr
import           Sound.OpenSoundControl (OSC)
import qualified Sound.OpenSoundControl as OSC
import qualified Sound.SC3 as SC
import           Sound.SC3.Server.Options
import           Sound.SC3.Server.Process (OutputHandler(..))

data InternalTransport = InternalTransport {
    world         :: MVar (Ptr C'World)     -- ^The World pointer, wrapped in an MVar.
  , recvChan      :: Chan OSC               -- ^The channel messages received from the library are written to.
  , replyFunc     :: C'ReplyFunc            -- ^Reply callback function pointer.
  , replyFuncData :: StablePtr (Chan OSC)   -- ^Data for the reply callback (a pointer to the receive channel).
  , printFunc     :: C'HaskellPrintFunc     -- ^Print callback function pointer.
  }

withInternal ::
    ServerOptions
 -> RTOptions
 -> OutputHandler
 -> (InternalTransport -> IO a)
 -> IO a
withInternal serverOptions rtOptions handler =
    bracket (withWorldOptions (newIT handler) serverOptions rtOptions)
            closeIT

newIT :: OutputHandler -> Ptr C'WorldOptions -> IO InternalTransport
newIT handler options = do
    pf <- mk'HaskellPrintFunc (\cs -> mapM_ (onPutString handler) . lines =<< peekCString cs)
    c'SetHaskellPrintFunc pf
    w <- newMVar =<< c'World_New options
    c <- newChan
    f <- mk'ReplyFunc it_replyFunc
    p <- newStablePtr c
    return $ InternalTransport w c f p pf

it_replyFunc :: Ptr C'ReplyAddress -> Ptr CChar -> CInt -> IO ()
it_replyFunc replyAddress cbuf csize = do
    -- putStrLn $ "it_replyFunc: " ++ show (replyAddress, cbuf, csize)
    buf <- BS.packCStringLen (cbuf, fromIntegral csize)
    ptr <- liftM castPtrToStablePtr (c'ReplyAddress_ReplyData replyAddress)
    chan <- deRefStablePtr ptr
    let osc = OSC.decodeOSC (BL.fromChunks [buf])
    -- putStrLn $ "it_replyFunc: " ++ show osc
    writeChan chan osc

copyChunks :: Ptr CChar -> [BS.ByteString] -> IO ()
copyChunks dst = foldM_ f 0
    where
        f i b = do
            let (fp, o, n) = BS.toForeignPtr b
            -- putStrLn $ "copyChunks " ++ show (i, o, n)
            withForeignPtr fp $ \src ->
                copyBytes (dst `plusPtr` i)
                          (src `plusPtr` o)
                          n
            return (i + n)

copyByteString :: Ptr CChar -> BL.ByteString -> IO ()
copyByteString dst = copyChunks dst . BL.toChunks

sendIT :: InternalTransport -> OSC -> IO ()
sendIT t osc = withMVar (world t) $ \w -> do
    let buf = OSC.encodeOSC osc
        n = BL.length buf
    when (w /= nullPtr) $ do
        -- TODO: Check return value and throw exception
        _ <- allocaArray (fromIntegral n) $ \cbuf -> do
            copyByteString cbuf buf
            c'World_SendPacketWithContext
                w
                (fromIntegral n)
                cbuf
                (replyFunc t)
                (castStablePtrToPtr (replyFuncData t))
        return ()

recvIT :: InternalTransport -> IO OSC
recvIT = readChan . recvChan

closeIT :: InternalTransport -> IO ()
closeIT t = do
    sendIT t SC.quit
    modifyMVar_ (world t) $ \w -> do
        when (w /= nullPtr) $ do
            c'World_WaitForQuit w
            freeHaskellFunPtr (replyFunc t)
            freeStablePtr (replyFuncData t)
            freeHaskellFunPtr (printFunc t)
        return nullPtr

withWorldOptions :: (Ptr C'WorldOptions -> IO a) -> ServerOptions -> RTOptions -> IO a
withWorldOptions f so ro = do
    (fs, cs) <- flip State.execStateT ([], []) $ do
        -- c'WorldOptions'mPassword :: CString
        setOpt (\x -> x { c'WorldOptions'mNumBuffers = int (numberOfSampleBuffers so) })
        setOpt (\x -> x { c'WorldOptions'mMaxLogins  = int (maxNumberOfLogins ro) })
        setOpt (\x -> x { c'WorldOptions'mMaxNodes   = int (maxNumberOfNodes so) })
        setOpt (\x -> x { c'WorldOptions'mMaxGraphDefs = int (maxNumberOfSynthDefs so) })
        setOpt (\x -> x { c'WorldOptions'mMaxWireBufs = int (numberOfWireBuffers so) })
        setOpt (\x -> x { c'WorldOptions'mNumAudioBusChannels = int (numberOfAudioBusChannels so) })
        setOpt (\x -> x { c'WorldOptions'mNumInputBusChannels = int (numberOfInputBusChannels so) })
        setOpt (\x -> x { c'WorldOptions'mNumOutputBusChannels = int (numberOfOutputBusChannels so) })
        setOpt (\x -> x { c'WorldOptions'mNumControlBusChannels = int (numberOfControlBusChannels so) })
        setOpt (\x -> x { c'WorldOptions'mBufLength = int (blockSize so) })
        setOpt (\x -> x { c'WorldOptions'mRealTimeMemorySize = int (realtimeMemorySize so) })
        -- TODO: Make shared controls accessible
        setOpt (\x -> x { c'WorldOptions'mNumSharedControls = 0 })
        setOpt (\x -> x { c'WorldOptions'mSharedControls = nullPtr })
        -- TODO
        -- tell (\x -> x { c'WorldOptions'mRealTime })
        -- TODO
        -- tell (\x -> x { c'WorldOptions'mMemoryLocking :: CInt
        -- tell (\x -> x { c'WorldOptions'mNonRealTimeCmdFilename :: CString
        -- tell (\x -> x { c'WorldOptions'mNonRealTimeInputFilename :: CString
        -- tell (\x -> x { c'WorldOptions'mNonRealTimeOutputFilename :: CString
        -- tell (\x -> x { c'WorldOptions'mNonRealTimeOutputHeaderFormat :: CString
        -- tell (\x -> x { c'WorldOptions'mNonRealTimeOutputSampleFormat :: CString
        setOpt (\x -> x { c'WorldOptions'mPreferredSampleRate = int (hardwareSampleRate ro) })
        setOpt (\x -> x { c'WorldOptions'mNumRGens = int (numberOfRandomSeeds so) })
        setOpt (\x -> x { c'WorldOptions'mPreferredHardwareBufferFrameSize = int (hardwareBufferSize ro) })
        setOpt (\x -> x { c'WorldOptions'mLoadGraphDefs = bool (loadSynthDefs so) })
        -- tell (\x -> x { c'WorldOptions'mInputStreamsEnabled :: CString
        -- tell (\x -> x { c'WorldOptions'mOutputStreamsEnabled :: CString
        -- TODO: Make input and output device configurable separately
        case hardwareDeviceName ro of
            Nothing -> return ()
            Just name -> do
                setOptS (\x s -> x { c'WorldOptions'mInDeviceName = s }) name
                setOptS (\x s -> x { c'WorldOptions'mOutDeviceName = s }) name
        setOpt (\x -> x { c'WorldOptions'mVerbosity = int (fromEnum (verbosity so)) })
        setOpt (\x -> x { c'WorldOptions'mRendezvous = bool (useZeroconf ro) })
        maybe (return ()) (setOptS (\x s -> x { c'WorldOptions'mUGensPluginPath = s }) . path) (ugenPluginPath so)
        maybe (return ()) (setOptS (\x s -> x { c'WorldOptions'mRestrictedPath = s })) (restrictedPath so)
    opts <- liftM (flip (foldl (flip ($))) fs) (c'kDefaultWorldOptions >>= peek)
    a <- alloca $ \ptr -> do
        ptr `poke` opts
        f ptr
    mapM_ free cs
    return a
    where
        int :: (Integral a, Num b) => a -> b
        int = fromIntegral
        bool :: Num a => Bool -> a
        bool b = int (if b then 1 else 0)
        path :: [String] -> String
        path = L.intercalate ":"
        setOpt f = do
            (fs, cs) <- State.get
            State.put (f:fs, cs)
        setOptS f s = do
            ptr <- liftIO (newCString s)
            (fs, cs) <- State.get
            State.put (flip f ptr:fs, ptr:cs)
            
instance OSC.Transport InternalTransport where
    send  = sendIT
    recv  = recvIT
    close = closeIT
